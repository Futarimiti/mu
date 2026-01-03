{
  description = "mu the music player";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskell.packages.ghc984;
      in
      {
        packages = rec {
          default = withConfig null;
          withConfig =
            conf:
            import ./default.nix {
              inherit pkgs haskellPackages conf;
            };
          example = withConfig (import ./nix/example.nix);
        };
        devShells.default = import ./shell.nix {
          inherit pkgs haskellPackages;
        };
      }
    );
}
