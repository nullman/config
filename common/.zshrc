#!/usr/bin/env zsh
#=============================================================================
# .zshrc
#
# Zsh Config
#
# Author: Kyle W T Sherman
#=============================================================================

# check if executable command is found in the path
_command() {
    command -v "$1" > /dev/null 2>&1
}

# keep original TERM value for scripts to use
export REAL_TERM="${TERM}"
# act like xterm with color support
export TERM="xterm-256color"

# only run if terminal is interactive
[[ "$-" == "*i*" ]] && return 0

# add to the run path
export runpath="${runpath}:${HOME}/.zshrc"
logger "Running: ${HOME}/.zshrc"

# set environmental vars
export SHELL="/bin/zsh"

# command history
export HISTFILE="${HOME}/.cache/zsh_history"
export HISTSIZE="10000"
export SAVEHIST="10000"
export HISTCONTROL="ignoredups"
export HISTORY_IGNORE="(ls|pwd|history|h|cd|cd -|cd ..|cdd|exit|reboot|sudo reboot)"

# use emacs key bindings
bindkey -e

# set options
setopt appendhistory       # immediately append history instead of overwriting
setopt autocd              # if only directory path is entered, cd there
setopt extendedglob        # extended globbing; allows using regular expressions with *
setopt histignorealldups   # if a new command is a duplicate, remove the older one
setopt histignorespace     # do not add commands starting with a space to the history file
setopt interactivecomments # use bash style comments
setopt nobeep              # no beep
setopt nocaseglob          # case insensitive globbing
setopt nocheckjobs         # do not warn about running processes when exiting
setopt nocorrect           # do not auto correct mistakes
setopt numericglobsort     # sort filenames numerically when it makes sense
setopt rcexpandparam       # array expension with parameters

# load terminfo
zmodload zsh/terminfo

# source auto-suggestions
[[ -f "/usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh" ]] && source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh 2>&1

# source syntax highlighting
[[ -f "/usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh" ]] && source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>&1

# source history substring
[[ -f "/usr/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh" ]] && source /usr/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh 2>&1

# source autojump
#[[ -f "/usr/share/autojump/autojump.zsh" ]] && source /usr/share/autojump/autojump.zsh 2>&1

# source powerlevel10k theme
[[ -f "${HOME}/powerlevel10k/powerlevel10k.zsh-theme" ]] && source ${HOME}/powerlevel10k/powerlevel10k.zsh-theme 2>&1

# fix home/end keys in screen/tmux
if [[ -n "${STY}" ]] || [[ -n "${TMUX}" ]] ; then
    bindkey "\e[1~" beginning-of-line
    bindkey "\e[4~" end-of-line
fi

# set prompt
if [[ -n "${INSIDE_EMACS}" ]] ; then
    # use simple prompt, if run from within emacs
    [[ -f "${HOME}/.p10k-simple.zsh" ]] && source ${HOME}/.p10k-simple.zsh 2>&1
    #export TERM=eterm-256color
    unset zle_bracketed_paste
else
    # (To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.)
    #[[ -f "${HOME}/.p10k.zsh" ]] && source ${HOME}/.p10k.zsh

    # enable powerlevel10k instant prompt
    [[ -f "${XDG_CACHE_HOME:-${HOME}/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]] && source "${XDG_CACHE_HOME:-${HOME}/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" 2>&1
fi

# compinstall
autoload -U compinit
zstyle :compinstall filename "${HOME}/.zshrc"
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' # case insensitive tab completion
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}" # colored completion (different colors for dirs/files/etc)
zstyle ':completion:*' rehash true # automatically find new executables in path
# speed up completions
zstyle ':completion:*' accept-exact '*(N)'
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "${HOME}/.cache/zsh"
zmodload zsh/complist
compinit
_comp_options+=(globdots) # include hidden files

# key bindings
bindkey '^ ' autosuggest-accept        # ctrl+space key to accept auto-suggestion
bindkey '^[[7~' beginning-of-line      # home key to move to beginning-of-line
bindkey '^[[H' beginning-of-line       # home key to move to beginning-of-line
[[ -n "${terminfo[khome]}" ]] && bindkey "${terminfo[khome]}" beginning-of-line # [home] to move to beginning-of-line
bindkey '^[[8~' end-of-line             # end key to move to end-of-line
bindkey '^[[F' end-of-line              # end key to move to end-of-line
[[ -n "${terminfo[kend]}" ]] && bindkey "${terminfo[kend]}" end-of-line # [end] to move to end-of-line
bindkey '^[[2~' overwrite-mode    # insert key to toggle overwrite mode
bindkey '^[[3~' delete-char       # delete key to delete one character forward
bindkey '^[[C' forward-char       # right key to move forward one character
bindkey '^[[D' backward-char      # left key to move backward one character
#bindkey '^[[5~' history-beginning-search-backward # page up key to search history backwards
#bindkey '^[[6~' history-beginning-search-forward # page down key to search history forwards

# navigate words with ctrl+arrow keys
bindkey '^[Oc' forward-word       # ctrl+right key to move forward one word
bindkey '^[Od' backward-word      # ctrl+left key to move backward one word
bindkey '^[[1;5D' backward-word   # ctrl+up key to move backward one word
bindkey '^[[1;5C' forward-word    # ctrl+down key to move forward one word
bindkey '^H' backward-kill-word   # ctrl+backspace key to delete previous word
bindkey '^[[Z' undo               # shift+tab key to undo

## bind up and down arrow keys to history substring search
#[[ -n "${terminfo[kcuu1]}" ]] && bindkey "${terminfo[kcuu1]}" history-substring-search-up # [up] to search substring history backwards
#[[ -n "${terminfo[kcud1]}" ]] && bindkey "${terminfo[kcud1]}" history-substring-search-down # [down] to search substring history forwards
#bindkey '^[[A' history-substring-search-up # up key to search substring history backwards
#bindkey '^[[B' history-substring-search-down # down to search substring history forwards

# bind page up and page down to history substring search
bindkey '^[[5~' history-substring-search-up # page up key to search history backwards
bindkey '^[[6~' history-substring-search-down # page down key to search history forwards
bindkey -M emacs '^P' history-substring-search-up # ctrl+p key to search history backwards
bindkey -M emacs '^N' history-substring-search-down # ctrl+n key to search history backwards

# stty commands should only run on interactive terminals
if [[ "$-" == "*i*" ]] ; then
    # make backspace key work in terminal
    #stty erase ^H
    #stty erase ^?
    #[[ -z "$(greppr erase)" ]] || stty erase $(getpr erase)

    # set tabs
    _command stty && stty tabs
fi

# set umask
umask 0022

# turn off console output warning
typeset -g POWERLEVEL9K_INSTANT_PROMPT="quiet"

# source shellrc
[[ -f "${HOME}/.shellrc" ]] && source ${HOME}/.shellrc 2>&1

#===============================================================================
# End of File
#===============================================================================
