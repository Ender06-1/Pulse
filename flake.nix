{
  description = "Pulse flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    { self, nixpkgs, ... }@inputs:
    let
      system = "x86_64-linux";

      pkgs = import nixpkgs { inherit system; };
    in
    {
      devShells.${system}.default = pkgs.mkShell {
        name = "pusle";

        packages = with pkgs; [
          ocaml
          dune_3

          ocamlPackages.ocaml-lsp
          ocamlPackages.ocamlformat
        ];
      };
    };
}
