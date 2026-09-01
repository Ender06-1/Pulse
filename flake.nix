{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = inputs@{ self, nixpkgs, ... }:
  let
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };
  in
  {
    devShells.${system}.default = pkgs.mkShell {
      name = "pulse";

      hardeningDisable = [ "fortify" ];

      packages = with pkgs; [
        nasm
        clang-tools
        gdb
      ];
    };
  };
}
