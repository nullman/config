#===============================================================================
# gcc-ia16-build-env.nix
#
# GCC IA16 NixOS Development Shell Environment
#
# Usage:
#
#   nix-shell ~/.nixos/gcc-ia16-build-env.nix
#
# Author: Kyle W T Sherman <kylewsherman@gmail.com>
#===============================================================================

{ pkgs ? import <nixpkgs> {} }:

let e = pkgs.buildFHSEnv {
      name = "gcc-ia16-build-env";
      pkgs.overlays = [
        (final: prev: {
          binutils-ia16 = prev.callPackage /home/kyle/.nixos/pkgs/binutils-ia16 {};
          gcc-ia16 = prev.callPackage /home/kyle/.nixos/pkgs/gcc-ia16 {};
        })
      ];
      targetPkgs = pkgs: with pkgs; [
        binutils-ia16
        gcc-ia16
        glibc
      ];
    };
in e.env

#===============================================================================
# End of File
#===============================================================================
