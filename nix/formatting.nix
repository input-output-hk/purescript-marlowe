{ writeShellScriptBinInRepoRoot
, pkgs
, easy-ps
}:
let
  inherit (builtins) concatStringsSep;
  inherit (pkgs.nodePackages) prettier;
  inherit (easy-ps) purs-tidy dhall-simple;
  inherit (pkgs) writeShellScriptBin;

  xargs = "${pkgs.findutils}/bin/xargs";

  extensionsToRegex = extensions: "\\.(${concatStringsSep "|" extensions})";

  writeFormatter = name: cmd: extensions: writeShellScriptBinInRepoRoot "fix-${name}" ''
    set -e
    echo formatting with ${name}
    ${pkgs.git}/bin/git ls-files ':!:bitte/node/config/*'\
      | grep -E '${extensionsToRegex extensions}' \
      | ${xargs} -d $'\\n' ${cmd}
    echo done.
  '';

  dhall-batch = writeShellScriptBin "dhall" ''
    for f in "$@"; do ${dhall-simple}/bin/dhall format --inplace $f; done
  '';
in
{
  fix-prettier = writeFormatter
    "prettier"
    "${prettier}/bin/prettier -w"
    [ "js" "ts" "css" "html" ];

  fix-purs-tidy = writeFormatter
    "purs-tidy"
    "${purs-tidy}/bin/purs-tidy format-in-place"
    [ "purs" ];

  fix-nix-fmt = writeFormatter
    "nixfmt"
    "${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt"
    [ "nix" ];


  purs-tidy-hook = {
    enable = true;
    name = "purs-tidy";
    entry = "${purs-tidy}/bin/purs-tidy format-in-place";
    files = "\\.purs$";
    language = "system";
  };

  prettier-hook = {
    enable = true;
    types_or = [ "javascript" "css" "html" ];
  };

  dhall-hook = {
    enable = true;
    name = "dhall";
    entry = "${dhall-batch}/bin/dhall";
    files = "\\.dhall$";
    language = "system";
  };
}
