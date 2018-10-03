let
  _pkgs = import <nixpkgs> {};
in
  import (_pkgs.fetchgit (builtins.fromJSON (builtins.readFile ./nixpkgs.json))) { }
