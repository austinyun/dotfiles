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

alias ..='cd ..'
alias ...='cd ../..'

alias gp='git push origin master'
alias gs='git st --short'
alias gl='git lg'
alias ga='git add'
alias gu='git add -u'
alias gc='git commit'

alias pacup='sudo pacman -Syu'

alias t='tmux attach -d'

# Environment variables -------------------------------------------------------
eval `dircolors ~/.dircolors` # Changes the LS_COLORS variable
export EDITOR='vim'
export GREP_OPTIONS='--color=auto'
export PATH="$PATH:$HOME/.gem/ruby/1.9.1/bin"

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

# Algorithms I stuff
export CLASSPATH=$CLASSPATH:$HOME/r/algs4/stdlib.jar:$HOME/r/algs4/algs4.jar
export PATH=$PATH:$HOME/r/algs4/bin
