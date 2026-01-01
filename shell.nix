{
  pkgs ? import <nixpkgs> { },
  haskellPackages ? pkgs.haskell.packages.ghc984,
}:
haskellPackages.shellFor {
  packages = ps: [ (ps.callCabal2nix "mu" ./. { }) ];
  nativeBuildInputs = [
    haskellPackages.haskell-language-server
    haskellPackages.cabal-install
    haskellPackages.hpack
    (import ./nix/runtime-deps.nix { inherit pkgs; })
  ];
  withHoogle = false;
}
