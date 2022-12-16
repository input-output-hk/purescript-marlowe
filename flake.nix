{
  description = "PureScript implementation of the Marlowe smart contract lanugage";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };
    marloweSpec.url = "github:input-output-hk/marlowe";
    nixLsp.url = "github:oxalica/nil";
    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, pre-commit-hooks, flake-utils, easy-purescript-nix, nixLsp, gitignore, marloweSpec }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          overlays = [ nixLsp.overlays.nil ];
          pkgs = import nixpkgs { inherit system overlays; };
          easy-ps = import easy-purescript-nix { inherit pkgs; };
          spagoPkgs = import ./spago-packages.nix { inherit pkgs; };

          inherit (gitignore.lib) gitignoreSource;
          inherit (easy-ps) spago psa purs;
          inherit (pkgs) git writeShellScriptBin nodePackages mkShell nodejs nixpkgs-fmt;
          inherit (nodePackages) bower prettier;

          src = gitignoreSource ./.;

          writeShellScriptBinInRepoRoot = name: script: writeShellScriptBin name ''
            cd `${git}/bin/git rev-parse --show-toplevel`
            ${script}
          '';

          formatting = import ./nix/formatting.nix {
            inherit writeShellScriptBinInRepoRoot pkgs easy-ps;
          };

          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            inherit src;
            hooks = {
              nixpkgs-fmt = {
                enable = true;
                excludes = [ ".*spago-packages.nix$" ];
              };
              prettier = formatting.prettier-hook;
              inherit (formatting) purs-tidy-hook dhall-hook;
            };
          };

          clean = writeShellScriptBinInRepoRoot "clean" ''
            echo cleaning project...
            rm -rf .spago .spago2nix output .psa-stash
            echo removed .spago
            echo removed .spago2nix
            echo removed .psa-stash
            echo removed output
            echo done.
          '';

          psa-args = "--strict --stash --censor-lib --is-lib=.spago";

          runSpago = cmd: ''
            ${spago}/bin/spago ${cmd} --purs-args "${psa-args} --stash"
          '';

          getGlob = { name, version, ... }: ''".spago/${name}/${version}/src/**/*.purs"'';

          spagoSources =
            builtins.toString
              (builtins.map getGlob (builtins.attrValues spagoPkgs.inputs));

          build = writeShellScriptBin "build" (runSpago "build");
          test = writeShellScriptBin "test" (runSpago "test");
          clean-build = writeShellScriptBin "clean-build" ''
            ${clean}/bin/clean
            ${build}/bin/build
          '';

          marloweSpecBin = marloweSpec.packages."${system}"."marlowe-spec-test:exe:marlowe-spec";

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
                set -e
                echo building project...
                psa compile ${psa-args} ${spagoSources} "./src/**/*.purs"
                echo done.
              '';
              installPhase = ''
                mkdir $out
                mv output $out/
              '';
              doCheck = true;
              checkPhase = ''
                set -e
                psa compile ${psa-args} ${spagoSources} "./src/**/*.purs" "./test/**/*.purs"
                node -e 'require("./output/Test.Main/index").main()'
              '';
            };

        in
        {
          defaultPackage = marlowe;
          devShell = mkShell {
            buildInputs = [
              build
              clean
              clean-build
              nixpkgs-fmt
              pkgs.nil
              marloweSpecBin
              test
              prettier
              bower
              nodejs
              easy-ps.dhall-simple
              easy-ps.purs
              easy-ps.spago
              easy-ps.spago2nix
              easy-ps.purs-tidy
              easy-ps.purescript-language-server
            ] ++
            (with formatting; [
              fix-purs-tidy
              fix-nix-fmt
              fix-prettier
            ]
            );
            inherit (pre-commit-check) shellHook;
          };
        }
      );
}
