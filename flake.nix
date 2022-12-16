{
  description = "PureScript implementation of the Marlowe smart contract language";

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
          inherit (gitignore.lib) gitignoreSource;
          inherit (pkgs) git writeShellScriptBin mkShell nodePackages;

          easy-ps = import easy-purescript-nix { inherit pkgs; };

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

          purescript-marlowe = import ./nix/purescript-marlowe.nix {
            inherit pkgs src easy-ps writeShellScriptBinInRepoRoot;
          };

          marloweSpecBin = marloweSpec.packages."${system}"."marlowe-spec-test:exe:marlowe-spec";
        in
        {
          packages.default = purescript-marlowe.marlowe;
          packages.generateSpagoPackages = purescript-marlowe.generateSpagoPackages;
          devShell = mkShell {
            buildInputs = [
              marloweSpecBin
              pkgs.nil
              pkgs.nodejs
              nodePackages.prettier
              nodePackages.bower
            ] ++
            (with easy-ps; [
              dhall-simple
              purs
              psa
              spago
              spago2nix
              purs-tidy
              purescript-language-server
            ]) ++
            (with purescript-marlowe; [
              build
              clean
              clean-build
              test
              generateSpagoPackages
            ]) ++
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
