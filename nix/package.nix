{
  generateOptparseApplicativeCompletions,
  developPackage,
  pkgs,
  symlinkJoin,
  makeWrapper,
  lib,
  writeText,
  conf ? null,
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
  configFileFlag = lib.optionalString (conf != null) ''
    --append-flags "--config ${writeText "config.json" (builtins.toJSON conf)}"
  '';
  withRuntimeDeps =
    deps: pkg:
    symlinkJoin {
      inherit name;
      paths = [ pkg ];
      buildInputs = [ makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/mu \
          --prefix PATH : ${lib.makeBinPath deps} \
          ${configFileFlag}
      '';
    };
in
withRuntimeDeps runtimeDeps (withCompletion muPkg)
