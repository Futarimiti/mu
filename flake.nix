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
          example = withConfig {
            playlists = [ ];
            library = [
              {
                name = "arcadia";
                url = "https://www.youtube.com/watch?v=e0LujX7wAQg";
              }
            ];
          };
        };
        devShells.default = import ./shell.nix {
          inherit pkgs haskellPackages;
        };
      }
    );
}
