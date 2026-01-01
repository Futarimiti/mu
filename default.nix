{
  pkgs ? import <nixpkgs> { },
  haskellPackages ? pkgs.haskell.packages.ghc984,
}:
haskellPackages.callPackage ./nix/package.nix { }
