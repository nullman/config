#!/usr/bin/env bash
#=============================================================================
# .bash_profile
#
# Remote Login Bash Config
#
# Author: Kyle W T Sherman
#=============================================================================

# add to the run path
export runpath="${runpath}:${HOME}/.bash_profile"
logger "Running: ${HOME}/.bash_profile"

# source bashrc
[[ -f "${HOME}/.bashrc" ]] && source "${HOME}/.bashrc"

#===============================================================================
# End of File
#===============================================================================
