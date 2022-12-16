{ pkgs
, src
, easy-ps
, writeShellScriptBinInRepoRoot
, marloweSpecDriver
}:
let
  inherit (easy-ps) spago psa purs spago2nix;
  inherit (pkgs) writeShellScriptBin nodejs;

  spagoPkgs = import ./spago-packages.nix { inherit pkgs; };

  psa-args = "--strict --stash --censor-lib --is-lib=.spago";

  getGlob = { name, version, ... }: ''".spago/${name}/${version}/src/**/*.purs"'';

  spagoSources =
    builtins.toString
      (builtins.map getGlob (builtins.attrValues spagoPkgs.inputs));

  clean = writeShellScriptBinInRepoRoot "clean" ''
    echo cleaning project...
    rm -rf .spago .spago2nix output .psa-stash
    echo removed .spago
    echo removed .spago2nix
    echo removed .psa-stash
    echo removed output
    echo done.
  '';

  generateSpagoPackages = writeShellScriptBinInRepoRoot "generate-spago2nix" ''
    n=0
    # spago2nix some times fail to fetch dependencies, if that happens,
    # retry up to 5 times
    until [ "$n" -ge 5 ]
    do
       ${spago2nix}/bin/spago2nix generate 5 -- -x spec-test.dhall && break
       n=$((n+1))
       echo "Retrying..."
    done

    mv spago-packages.nix nix/spago-packages.nix
  '';

  build = writeShellScriptBinInRepoRoot "marlowe-build" ''
    set -e
    if [ ! -d ".spago" ]; then
      ${spagoPkgs.installSpagoStyle}/bin/install-spago-style
    fi

    echo building project...
    ${psa}/bin/psa ${psa-args} ${spagoSources} "./src/**/*.purs"
    echo done.
  '';

  test = writeShellScriptBinInRepoRoot "marlowe-test" ''
    set -e
    if [ ! -d ".spago" ]; then
      ${spagoPkgs.installSpagoStyle}/bin/install-spago-style
    fi
    ${psa}/bin/psa ${psa-args} ${spagoSources} "./src/**/*.purs" "./spec-test/**/*.purs"
    node -e 'import("./output/Test.Main/index.js").then(module => module.main())'
    ${marloweSpecDriver}/bin/marlowe-spec -c ${marlowe-spec-client}/bin/marlowe-spec-client
  '';

  clean-build = writeShellScriptBin "clean-build" ''
    ${clean}/bin/clean
    ${build}/bin/marlowe-build
  '';

  marlowe-spec-client = writeShellScriptBinInRepoRoot "marlowe-spec-client" ''
    ${nodejs}/bin/node spec-client.mjs
  '';
in
{
  inherit build test clean-build clean generateSpagoPackages marlowe-spec-client;
  marlowe =
    pkgs.stdenv.mkDerivation {
      name = "purescript-marlowe";
      buildInputs = [
        spagoPkgs.installSpagoStyle
      ];
      nativeBuildInputs = [ psa purs nodejs ];
      inherit src;
      unpackPhase = ''
        cp -r $src/* .
        install-spago-style
      '';
      buildPhase = ''
        ${build}/bin/marlowe-build
      '';
      installPhase = ''
        mkdir $out
        mv output $out/
      '';
      doCheck = true;
      checkPhase = ''
        ${test}/bin/marlowe-test
      '';
    };
}
