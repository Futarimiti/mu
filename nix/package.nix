{
  generateOptparseApplicativeCompletions,
  developPackage,
  pkgs,
  symlinkJoin,
  makeWrapper,
  lib,
  ...
}:
let
  muPkg = developPackage {
    root = ../.;
    returnShellEnv = false;
  };
  name = "mu";
  runtimeDeps = import ./runtime-deps.nix { inherit pkgs; };
  withCompletion = generateOptparseApplicativeCompletions [ name ];
  withRuntimeDeps =
    deps: pkg:
    symlinkJoin {
      inherit name;
      paths = [ pkg ];
      buildInputs = [ makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/mu --prefix PATH : ${lib.makeBinPath deps}
      '';
    };
in
withRuntimeDeps runtimeDeps (withCompletion muPkg)
