export EDITOR='vim'
mkcd() {
        if [ $# != 1 ]; then
                echo "Usage: mkcd <dir>"
        else
                mkdir -p $1 && cd $1
        fi
}

# TODO: Make this work without re-sourcing
cd() {
      if [ -d "${@: -1}" ] ; then
          builtin cd $*;
      else
          $EDITOR "${@: -1}"
      fi
}

bind -r '\C-s'
stty -ixon

alias l='ls -A'
alias lah='ls -lAh'
alias lf='ls -CF'

alias ..='cd ..'
alias cls='clear'

# git aliases
alias gs='git status'
alias gb='git branch'
alias ga='git add'
alias gc='git commit -m '
#git h is set up in .gitconfig
alias gh='git h'

# Python Virtualenv aliases
alias vc='virtualenv venv'
alias va='source venv/bin/activate'

alias pyclean="find . -name '*.pyc' -delete"
alias pimp="python manage.py"

# Tmux
alias tks='tmux kill-session'
alias tls='tmux ls'

# Scheme with niceties
alias scheme="rlwrap -r -c -f ~/.config/mit_scheme_bindings.txt scheme"

#Add RVM to PATH for scripting
PATH=$PATH:/usr/local/rvm/bin
### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"
#for Homebrew
export PATH="/usr/local/bin:$PATH"

[[ -s "/usr/local/etc/bash_completion.d/git-completion.bash" ]] && source /usr/local/etc/bash_completion.d/git-completion.bash
[[ -s "/usr/local/etc/bash_completion.d/git-flow-completion.bash" ]] && source /usr/local/etc/bash_completion.d/git-flow-completion.bash
