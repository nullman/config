#===============================================================================
# ruby-env.nix
#
# Ruby Script Shell Environment
#
# Usage:
#
#   nix-shell ~/.nixos/ruby-env.nix
#
# Author: Kyle W T Sherman <kylewsherman@gmail.com>
#===============================================================================

{ pkgs ? import <nixpkgs> {} }:

let
  ruby = pkgs.ruby;
  bundlerEnv = pkgs.bundlerEnv {
    name = "ruby-env";
    ruby = ruby;
    gemfile = ./Gemfile;
    lockfile = ./Gemfile.lock;
    gemset = ./gemset.nix;
  };

in pkgs.mkShell {
  buildInputs = [
    bundlerEnv
  ];
}

#===============================================================================
# End of File
#===============================================================================
