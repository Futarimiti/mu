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
        mu = import ./default.nix { inherit pkgs haskellPackages; };
      in
      {
        packages.default = mu;
        apps.default = flake-utils.lib.mkApp { drv = mu; };
        devShells.default = import ./shell.nix { inherit pkgs haskellPackages; };
      }
    );
}
