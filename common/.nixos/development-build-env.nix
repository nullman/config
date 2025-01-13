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
        #libGL
        libmpc
        libxcrypt
        lua
        mpfr
        ncurses ncurses.dev
        readline readline.dev
        SDL2 SDL2.dev
        SDL2_mixer SDL2_mixer.dev
        xz xz.dev
        zlib zlib.dev

        # xorg
        xorg.libX11 xorg.libX11.dev
        xorg.libxcb xorg.libxcb.dev
        xorg.libXext xorg.libXext.dev
        xorg.libXinerama xorg.libXinerama.dev
        xorg.libXrandr xorg.libXrandr.dev
        xorg.libXt xorg.libXt.dev
        xorg.xcbutil xorg.xcbutil.dev
        xorg.xcbutilkeysyms xorg.xcbutilkeysyms.dev
        xorg.xcbutilwm xorg.xcbutilwm.dev
        xorg.xorgproto

        # toolchain
        asciidoc
        bison
        flex
        gcc
        m4
        perl
        pkgconf
        stdenv.cc
        stdenv.cc.libc stdenv.cc.libc_dev
        texinfo
        xorg.imake

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
