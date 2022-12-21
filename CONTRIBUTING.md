# Contributing guide

In order to contribute to this project, it is necesary to have [Nix](https://nixos.org/) installed and [Nix Flakes](https://nixos.wiki/wiki/Flakes) enabled.

> Note: Make sure to have the binary caches configured to avoid large builds.

Once installed, you can enter the nix shell by calling

```bash
$ nix develop
```

Inside the nix shell, you will have access to the correct version of PureScript, Node.js and other dependencies.

You can build the project by executing

```bash
[nix-shell] $ marlowe-build
```

You can run the test by executing

```bash
[nix-shell] $ marlowe-test
```

You can generate the documention by executing

```bash
[nix-shell] $ build-docs
```

And serve a local version with

```bash
[nix-shell] $ serve-docs
```

## Configure binary cache

Add the following to your nix configuration:

```
substituters        = https://cache.iog.io https://iohk.cachix.org https://cache.nixos.org/
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
Note

```

## Update spago2nix

The spago dependencies are tracked with [spago2nix](https://github.com/justinwoo/spago2nix). If any purescript dependency change, we need to run the following:

```bash
$ nix run .#generateSpagoPackages
```

## Update node2nix

The npm dependencies are tracked with [node2nix](https://github.com/svanderburg/node2nix). If any dependency is added to package.json, we need to run the following:


```bash
$ nix run .#generateNpmPackages
```