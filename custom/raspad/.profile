#!/usr/bin/env bash
#=============================================================================
# .profile
#
# Raspad Initialization Configuration
#
# Author: Kyle W T Sherman
#=============================================================================

# add to the run path
export runpath="${runpath}:${HOME}/.profile"
logger "Running: ${HOME}/.profile"

# restore rotation
[[ -x ${HOME}/.raspad-rotate-restore ]] && ${HOME}/.raspad-rotate-restore

#=============================================================================
# End of File
#=============================================================================
