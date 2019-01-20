# export EDITOR='vim'
export EDITOR='emacsclient -t'
alias emacs='emacsclient --alternate-editor="" -t'

BASE16_SHELL="$HOME/.config/base16-shell/base16-tomorrow.dark.sh"
[[ -s $BASE16_SHELL ]] && source $BASE16_SHELL


# Custom bash prompt via kirsle.net/wizards/ps1.html
# export PS1="\[$(tput bold)\]\[$(tput setaf 7)\]\A>\[$(tput setaf 1)\]\h:\[$(tput setaf 2)\]\W \\$ \[$(tput sgr0)\]"
# Prompt with git branch on the end
function parse_git_branch {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1) /'
}

export PS1="\[$(tput setaf 7)\]\A\[$(tput setaf 1)\]❯\[$(tput setaf 3)\]❯\[$(tput setaf 2)\]❯ \[$(tput setaf 1)\]\u:\[$(tput setaf 2)\]\W \$(parse_git_branch)\\[$(tput setaf 4)\]\[$(tput setaf 2)\]$ \[$(tput sgr0)\]"


export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagaced

mkcd() {
        if [ $# != 1 ]; then
                echo "Usage: mkcd <dir>"
        else
                mkdir -p $1 && cd $1
        fi
}

bind -r '\C-s'
stty -ixon
set -o vi

alias ls='ls --color'
alias l='ls -A'
alias lah='ls -lAh'
alias lf='ls -d */'
alias lsd='ls -d */'

alias ..='cd ..'
alias cls='clear'

# git aliases
alias gs='git status'
alias gb='git branch'
alias ga='git add'
alias gc='git commit -m '
#git h is set up in .gitconfig
alias gh='git h'
alias master='git checkout master'
alias dev='git checkout develop'

# default C settings alias
alias ce='cc -Wall -Wextra -pedantic -Wstrict-aliasing -g -std=c99'
# Python Virtualenv aliases
alias vc='virtualenv venv'
alias va='source venv/bin/activate'

source /usr/local/bin/virtualenvwrapper.sh

alias pyclean="find . -name '*.pyc' -delete"
alias pimp="python manage.py"

# Tmux
alias tmux='tmux -2' # Force 256 color mode
alias tks='tmux kill-session'
alias tls='tmux ls'
alias tas='tmux attach-session -t'

alias mux='tmuxinator'

# Scheme with niceties
alias scheme="rlwrap -r -c -f ~/.config/mit_scheme_bindings.txt scheme"

#for Homebrew
export PATH="/usr/local/bin:$PATH"

[[ -s "/usr/local/etc/bash_completion.d/git-completion.bash" ]] && source /usr/local/etc/bash_completion.d/git-completion.bash
[[ -s "/usr/local/etc/bash_completion.d/git-flow-completion.bash" ]] && source /usr/local/etc/bash_completion.d/git-flow-completion.bash

export NVM_DIR="$HOME/.nvm"
# [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
if [[ $- == *i* ]]; then
    source $NVM_DIR/nvm.sh --no-use
else
    source $NVM_DIR/nvm.sh
fi
