#===============================================================================
# shell.nix
#
# General NixOS Development Shell Environment
#
# Usage:
#
#   nix-shell ~/.nixos/shell.nix
#
# Source: https://discourse.nixos.org/t/development-on-gcc-on-nixos/18376/4
#
# Author: Kyle W T Sherman <kylewsherman@gmail.com>
#===============================================================================

{ pkgs ? import <nixpkgs> {} }:

let e =
  pkgs.buildFHSEnv {
    name = "development-build-env";
    targetPkgs = ps: with ps; [
      # libraries
      gmp gmp.dev
      isl
      libffi libffi.dev
      libmpc
      libxcrypt
      mpfr mpfr.dev
      xz xz.dev
      zlib zlib.dev

      # utilities
      bison
      flex
      m4
      texinfo

      # test harnesses
      autogen
      dejagnu

      # valgrind annotations
      valgrind valgrind.dev

      # toolchain
      gcc
      stdenv.cc
      stdenv.cc.libc stdenv.cc.libc_dev
    ];
  };
in e.env

#===============================================================================
# End of File
#===============================================================================
