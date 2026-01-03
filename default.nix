{
  pkgs ? import <nixpkgs> { },
  haskellPackages ? pkgs.haskell.packages.ghc984,
  conf ? null,
}:
haskellPackages.callPackage ./nix/package.nix { inherit conf; }
