{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hsPkgs = pkgs.haskell.packages."ghc982";
      in
      {
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            stack
            hsPkgs.ghc
            hsPkgs.haskell-language-server
          ];
        };
      }
    );
}
