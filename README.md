# mincc

_Mincc: Mincc Is Not a Complex Compiler_

A compiler that transforms MinCaml sources into RINANA, an original assembly language inspired by RISC-V.

## Requirements

- GHC 9.8.2 or later
- stack 2.15.7 or later

Install these components by following the instructions on [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/). Installing it through ghcup is recommended.

## Usage

The basic usage of `mincc` is as follows:
```bash
mincc -i <input file> -o <output file>
```

To enable optimizations, you can use the `-O` option.
```bash
mincc -i <input file> -o <output file> -O
```

`mincc` has various options. For more information, use the `--help` option to get the list of them.
```bash
mincc --help
```

## Installation

### Run without installation

```bash
git clone https://github.com/cpuex1/mincc
cd mincc
stack run -- [options]
```

### Build it manually

Run the following commands to build `mincc` manually.

```bash
git clone https://github.com/cpuex1/mincc
cd mincc
stack --local-bin-path . build --copy-bins
```

An executable named `mincc` will be appeared in the current directory.

### Download artifacts from GitHUb

Visit [mincc build workflow](https://github.com/cpuex1/mincc/actions/workflows/build.yml) and download the artifacts.
