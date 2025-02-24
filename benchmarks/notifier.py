import json
from PIL import Image
from pathlib import Path
from datetime import datetime
import requests
import sys
import hashlib
import re

CONFIG_FILE = "notifier-config.json"

#
# Load the configurations.
#
saved_file = sys.argv[1]
with open(CONFIG_FILE, "r") as f:
    obj = json.load(f)
    webhook = obj["webhook"]
    avatar = obj["avatar"]
    output = obj["output"]
    status = obj["status"]
    width = int(obj["width"])
    height = int(obj["height"])

print(f"[!] Webhook: {webhook}")

#
# Calculate hash.
#
with open(saved_file, "rb") as f:
    ppm_hash = hashlib.sha256(f.read()).hexdigest()
    print(f"[!] PPM hash: {ppm_hash}")

#
# Load the status file.
#
with open(status, "r") as f:
    status_text = f.read()

    inst_count_regex = re.compile(r"実行命令数: (\d+)")
    matched = inst_count_regex.match(status_text)
    if matched:
        inst_count = int(matched.group(1))
        print(f"[!] Instruction count: {inst_count}")
    else:
        inst_count = -1
        print("[!] Instruction count not found")

    exec_regex = re.compile(r"時間: (\d+(\.\d+)?)秒")
    matched = exec_regex.search(status_text)
    if matched:
        exec_time = float(matched.group(1))
        print(f"[!] Execution time: {exec_time}")
    else:
        exec_time = -1
        print("[!] Execution time not found")

    inst_regex = re.compile(r"(\w+)\s+(\d+)times\s+ratio:\s+(\d+(\.\d+)?(e\-?\d+)?)")
    matched = inst_regex.finditer(status_text)
    inst_result = list(
        map(
            lambda m: {
                "name": str(m.group(1)),
                "value": f"{int(m.group(2)):,} ({float(m.group(3))}%)",
                "inline": "true",
            },
            sorted(matched, key=lambda m: int(m.group(2)), reverse=True),
        )
    )

#
# Resize the image.
#
png_file = Path(datetime.now().strftime(output))
with Image.open(saved_file) as img:
    print(f"[!] Loaded {saved_file}")

    (img_width, img_height) = img.size
    img = img.resize((width, height))
    print(f"[!] Resize image to {width}x{height}")

    img.save(png_file)
    print(f"[!] The image was saved as {png_file}")


#
# Send a message.
#
send_object = {
    "username": "Minrt notifier",
    "avatar_url": avatar,
    "embeds": [
        {
            "color": 0x00FF00,
            "title": "Execution result",
            "fields": [
                {
                    "name": "Size",
                    "value": f"{img_width}x{img_height}",
                    "inline": "false",
                },
                {"name": "Hash", "value": ppm_hash, "inline": "false"},
                {"name": "Inst num", "value": f"{inst_count:,}", "inline": "false"},
                {"name": "Exec", "value": f"{exec_time}s", "inline": "false"},
            ],
            "image": {"url": f"attachment://{png_file.name}"},
        },
        {
            "color": 0x909090,
            "title": "Details",
            "fields": inst_result[0:25],
        },
    ],
}
files = {
    "payload_json": (
        None,
        json.dumps(send_object),
    ),
    png_file.name: open(png_file, "rb"),
    status: open(status, "rb"),
}
res = requests.post(webhook, files=files)
print(f"[!] Response: {res.text}")
print("[!] Sent a message")
