#!/usr/bin/env bash
#===============================================================================
# .bashrc
#
# Bash Config
#
# Author: Kyle W T Sherman <kylewsherman@gmail.com>
#===============================================================================

# check if executable command is found in the path
_command() {
    command -v "$1" >/dev/null 2>&1
}

# shell colors
export COLOR_DEFAULT="\[\033[0m\]"      # light yellow
export COLOR_BLACK="\[\033[0;30m\]"
export COLOR_DARK_GRAY="\[\033[1;30m\]"
export COLOR_RED="\[\033[0;31m\]"
export COLOR_LIGHT_RED="\[\033[1;31m\]"
export COLOR_GREEN="\[\033[0;32m\]"
export COLOR_LIGHT_GREEN="\[\033[1;32m\]"
export COLOR_BROWN="\[\033[0;33m\]"
export COLOR_YELLOW="\[\033[1;33m\]"
export COLOR_LIGHT_YELLOW="\[\033[0m\]"
export COLOR_BLUE="\[\033[0;34m\]"
export COLOR_LIGHT_BLUE="\[\033[1;34m\]"
export COLOR_PURPLE="\[\033[0;35m\]"
export COLOR_LIGHT_PURPLE="\[\033[1;35m\]"
export COLOR_CYAN="\[\033[0;36m\]"
export COLOR_LIGHT_CYAN="\[\033[1;36m\]"
export COLOR_LIGHT_GRAY="\[\033[0;37m\]"
export COLOR_WHITE="\[\033[1;37m\]"

# source system bashrc
[[ -f "/etc/bashrc" ]] && source "/etc/bashrc" 2>&1

# keep original TERM value for scripts to use
export REAL_TERM="${TERM}"
# act like xterm with color support
export TERM="xterm-256color"

# continue only if terminal is interactive
[[ $- == *i* ]] || return 0

# add to the run path
export runpath="${runpath}:${HOME}/.bashrc"
logger "Running: ${HOME}/.bashrc"

# operating system
os="$(uname -s)"

# set environmental vars
export SHELL="$(command -v bash)"

if [[ -z "${INSIDE_EMACS}" ]] ; then
    # auto-completion: ignore case
    bind "set completion-ignore-case on"
    # auto-completion: single tab list
    bind "set show-all-if-ambiguous on"
fi

# command history
export HISTFILE="${HOME}/.cache/bash_history"
export HISTSIZE="10000"
export SAVEHIST="10000"
export HISTCONTROL="erasedups:ignoredups:ignorespace"
export HISTORY_IGNORE="(ls|pwd|history|h|cd|cd -|cd ..|cdd|exit|reboot|sudo reboot)"
shopt -s histappend    # allow multiple terminals to write to the history file
PROMPT_COMMAND='history -a'  # this terminal should append to the history file

# enable fzf fuzzy matching
if [[ -z "${INSIDE_EMACS}" ]] ; then
    if [[ "${os}" == "Darwin" ]] ; then
        [[ -d "/usr/local/opt/fzf/bin" ]] && [[ ! "${PATH}" == */usr/local/opt/fzf/bin* ]] && export PATH="${PATH}:/usr/local/opt/fzf/bin"
        [[ -f "/usr/local/opt/fzf/shell/key-bindings.bash" ]] && source "/usr/local/opt/fzf/shell/key-bindings.bash" 2>&1
        [[ -f "/usr/local/opt/fzf/shell/completion.bash" ]] && source "/usr/local/opt/fzf/shell/completion.bash" 2>&1
    elif $(uname -v | grep -q 'NixOS') ; then
        if _command fzf-share ; then
            source "$(fzf-share)/key-bindings.bash"
            source "$(fzf-share)/completion.bash"
        fi
    else
        [[ -f "/usr/share/fzf/key-bindings.bash" ]] && source "/usr/share/fzf/key-bindings.bash" 2>&1
        [[ -f "/usr/share/fzf/completion.bash" ]] && source "/usr/share/fzf/completion.bash" 2>&1
    fi
fi

# fix home/end keys in screen/tmux
if [[ -n "${STY}" ]] || [[ -n "${TMUX}" ]] ; then
    bind '"\e[1~":"\eOH"'
    bind '"\e[4~":"\eOF"'
fi

# set prompt
if [[ -n "${INSIDE_EMACS}" ]] ; then
    # set emacs prompt to: path $
    #export PS1="\w \\$ "
    # set emacs prompt to: path git-branch $
    export PS1="\w\$(git-branch-prompt) \\$ "
else
    # set terminal prompt to: host:user path $
    #export PS1="${COLOR_YELLOW}\h:\u ${COLOR_LIGHT_BLUE}\w ${COLOR_YELLOW}\\$ ${COLOR_DEFAULT}"
    # set terminal prompt to: host:user path git-branch $
    export PS1="${COLOR_YELLOW}\h:\u ${COLOR_LIGHT_BLUE}\w${COLOR_GREEN}\$(git-branch-prompt) ${COLOR_YELLOW}\\$ ${COLOR_DEFAULT}"
fi

# other prompts
# set prompt to: host:user path $
#export PS1="\h:\u \w \\$ "
# set prompt to: host:user path $ [with colors]
#export PS1="\[\033[01;32m\]\h:\u \[\033[01;34m\]\w \\$ \[\033[00m\]"
# set prompt to: host:user(window) path $
#export PS1="\[\033[01;32m\]\h:\u(${WINDOW:+${WINDOW}}) \[\033[01;34m\]\w \\$ \[\033[00m\]"
# set terminal window titles
#export PS1="${PS1}\[\e]0;\h:\u:\w\a\]"
# manjaro i3
#export PS1="\[\033[1;32m\]\h:\u \[\033[1;34m\]\w \[\033[1;33m\]\$ \[\033[0m\]"

# make backspace key work in terminal
#stty erase ^H
#stty erase ^?
#[[ -z "$(greppr erase)" ]] || stty erase $(getpr erase)

# set tabs
_command stty && stty tabs

# make terminals not beep
[[ "${os}" == "Windows_NT" ]] || _command setterm && setterm -blength 0 >/dev/null 2>&1

# set umask
umask 0022

# turn on auto cd
shopt -s autocd

# check window size
shopt -s checkwinsize

# source shellrc
[[ -f "${HOME}/.shellrc" ]] && source "${HOME}/.shellrc"

# run bash completion
[[ -f "/etc/bash-completion" ]] && source "/etc/bash-completion" 2>&1

# perl modules
#PATH="${HOME}/perl5/bin${PATH:+:${PATH}}"; export PATH;
#PERL5LIB="${HOME}/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
#PERL_LOCAL_LIB_ROOT="${HOME}/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
#PERL_MB_OPT="--install_base \"${HOME}/perl5\""; export PERL_MB_OPT;
#PERL_MM_OPT="INSTALL_BASE=${HOME}/perl5"; export PERL_MM_OPT;

#===============================================================================
# End of File
#===============================================================================
