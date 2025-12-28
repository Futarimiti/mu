{
  description = "mu the music player";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskell.packages.ghc984;
      in
      {
        packages = rec {
          mu = haskellPackages.generateOptparseApplicativeCompletions [ "mu" ] (
            haskellPackages.developPackage {
              root = ./.;
              returnShellEnv = false;
            }
          );
          default = mu;
        };
        apps = rec {
          mu = flake-utils.lib.mkApp { drv = self.packages.${system}.mu; };
          default = mu;
        };
        devShells = {
          default = haskellPackages.shellFor {
            packages = ps: [ (ps.callCabal2nix "mu" ./. { }) ];
            nativeBuildInputs = with haskellPackages; [
              haskell-language-server
              cabal-install
              hpack
            ];
            withHoogle = false;
          };
        };
      }
    );
}
