{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
  };

  outputs = inputs@{ self, nixpkgs, ... }:
  let
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };
  in
  {
    devShells.${system}.default = pkgs.mkShell {
      name = "pulse";
      packages =
        with pkgs;
        with pkgs.ocamlPackages; [
          ocaml
          ocaml-lsp
          ocamlformat
          findlib
          dune_3

          ppx_deriving

          fasm

          self.packages.${system}.pulse
        ];
    };

    packages.${system}.pulse = pkgs.ocamlPackages.buildDunePackage (finalAttrs: {
      pname = "pulse";
      version = "0.1.0";

      buildInputs = with pkgs.ocamlPackages; [
        ppx_deriving
      ];

      nativeBuildInputs = with pkgs; [
        fasm
      ];

      src = ./.;
    });
  };
}
