autoload -Uz vcs_info
autoload -U colors && colors

# Prompt ----------------------------------------------------------------------
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' check-for-changes true

zstyle ':vcs_info:*' stagedstr '%F{yellow%}'
zstyle ':vcs_info:*' unstagedstr '%F{red%}'
zstyle ':vcs_info:git*' formats '%u%b %c'

precmd () { vcs_info }

setopt prompt_subst
PROMPT='${vcs_info_msg_0_}[%~]%{$reset_color%} '

# Useful aliases --------------------------------------------------------------
alias ls='ls -F --color'
alias la='ls -A'
alias c='clear'

# Environment variables -------------------------------------------------------
export EDITOR='vim'
export GREP_OPTIONS='--color=auto'
