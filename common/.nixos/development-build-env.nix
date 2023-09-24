#===============================================================================
# development-build-env.nix
#
# General NixOS Development Shell Environment
#
# Usage:
#
#   nix-shell ~/.nixos/development-build-env.nix
#
# Source: https://discourse.nixos.org/t/development-on-gcc-on-nixos/18376/4
#
# Author: Kyle W T Sherman <kylewsherman@gmail.com>
#===============================================================================

{ pkgs ? import <nixpkgs> {} }:

let e = pkgs.buildFHSEnv {
      name = "development-build-env";
      targetPkgs = pkgs: with pkgs; [
        # libraries
        glibc
        gmp gmp.dev
        isl
        libffi libffi.dev
        libmpc
        libxcrypt
        mpfr mpfr.dev
        xz xz.dev
        zlib zlib.dev

        # toolchain
        bison
        flex
        gcc
        m4
        perl
        pkgconf
        stdenv.cc
        stdenv.cc.libc stdenv.cc.libc_dev
        texinfo

        # test harnesses
        autogen
        dejagnu

        # valgrind annotations
        valgrind valgrind.dev
      ];
    };
in e.env

#===============================================================================
# End of File
#===============================================================================
