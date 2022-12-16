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
    let
      inherit (flake-utils.lib) eachSystem system;
      supportedSystems = [ system.x86_64-linux system.x86_64-darwin ];

      base = { evalSystem ? null }: eachSystem supportedSystems
        (system:
          let evalSystem' = if evalSystem == null then system else evalSystem; in
          let
            evalSystem = evalSystem';
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
              inherit pkgs src easy-ps writeShellScriptBinInRepoRoot marloweSpecDriver;
            };

            marloweSpecDriver = marloweSpec.packages."${system}"."marlowe-spec-test:exe:marlowe-spec";

            packages = {
              default = purescript-marlowe.marlowe;
              generateSpagoPackages = purescript-marlowe.generateSpagoPackages;
            };
          in
          {
            inherit packages;
            hydraJobs = packages;

            devShell = mkShell {
              buildInputs = [
                marloweSpecDriver
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
                marlowe-spec-client
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
      hydraSystem = "x86_64-linux";
      pkgsHydra = nixpkgs.legacyPackages.${hydraSystem};
      baseHydra = base { evalSystem = hydraSystem; };
    in
    base { } // {
      hydraJobs = baseHydra.hydraJobs // {
        forceNewEval = pkgsHydra.writeText "forceNewEval" (self.rev or self.lastModified);
        required = pkgsHydra.releaseTools.aggregate {
          name = "purescript-marlowe";
          constituents = builtins.concatMap (system: map (x: "${x}.${system}") (builtins.attrNames baseHydra.hydraJobs)) supportedSystems;
        };
      };
    };
}
