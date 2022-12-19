{ pkgs
, src
, easy-ps
, writeShellScriptBinInRepoRoot
, marloweSpecDriver
}:
let
  inherit (easy-ps) spago psa purs spago2nix;
  inherit (pkgs) writeShellScriptBin nodejs;
  inherit (pkgs.nodePackages) node2nix;

  spagoPkgs = import ./spago-packages.nix { inherit pkgs; };

  nodePkgs = import ./node { inherit pkgs; };

  psa-args = "--strict --stash --censor-lib --is-lib=.spago";

  getGlob = { name, version, ... }: ''".spago/${name}/${version}/src/**/*.purs"'';

  spagoSources =
    builtins.toString
      (builtins.map getGlob (builtins.attrValues spagoPkgs.inputs));

  clean = writeShellScriptBinInRepoRoot "clean" ''
    echo cleaning project...
    rm -rf .spago .spago2nix output .psa-stash node_modules generated-docs
    echo removed .spago
    echo removed .spago2nix
    echo removed output
    echo removed .psa-stash
    echo removed node_modules
    echo removed generated-docs
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

  generateNpmPackages = writeShellScriptBinInRepoRoot "generate-node2nix" ''
    ${node2nix}/bin/node2nix \
      --output nix/node/registry.nix \
      --composition nix/node/default.nix \
      --node-env nix/node/node-env.nix \
      -18 \
      -l
  '';

  build = writeShellScriptBin "marlowe-build" ''
    set -e
    if [ ! -d ".spago" ]; then
      ${spagoPkgs.installSpagoStyle}/bin/install-spago-style
    fi

    echo building project...
    ${psa}/bin/psa ${psa-args} ${spagoSources} "./src/**/*.purs"
    echo done.
  '';

  test = writeShellScriptBin "marlowe-test" ''
    set -e
    if [ ! -d ".spago" ]; then
      ${spagoPkgs.installSpagoStyle}/bin/install-spago-style
    fi
    ${psa}/bin/psa ${psa-args} ${spagoSources} "./src/**/*.purs" "./spec-test/**/*.purs"

    echo "Local tests"
    ${nodejs}/bin/node -e 'import("./output/Test.Main/index.js").then(module => module.main())'

    echo "Marlowe Spec tests"
    ${marloweSpecDriver}/bin/marlowe-spec -c ${marlowe-spec-client}/bin/marlowe-spec-client
  '';

  clean-build = writeShellScriptBin "clean-build" ''
    ${clean}/bin/clean
    ${build}/bin/marlowe-build
  '';

  build-docs = writeShellScriptBinInRepoRoot "build-docs" ''
    ${spago}/bin/spago docs
  '';

  serve-docs = writeShellScriptBinInRepoRoot "serve-docs" ''
    if [ ! -d "generated-docs" ]; then
      ${build-docs}/bin/build-docs
    fi

    cd generated-docs/html
    ${nodejs}/bin/npx http-server -g -o
  '';

  marlowe-spec-client = writeShellScriptBin "marlowe-spec-client" ''
    ${nodejs}/bin/node spec-client.mjs
  '';
in
{
  inherit
    build
    test
    clean-build
    clean
    build-docs
    serve-docs
    generateSpagoPackages
    marlowe-spec-client
    generateNpmPackages
    ;

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
        ln -s ${nodePkgs.nodeDependencies}/lib/node_modules ./node_modules
        export PATH="${nodePkgs.nodeDependencies}/bin:$PATH"
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
