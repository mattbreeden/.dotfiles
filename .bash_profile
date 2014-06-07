[[ -s ~/.bashrc ]] && source ~/.bashrc
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

# Custom bash prompt via kirsle.net/wizards/ps1.html
# export PS1="\[$(tput bold)\]\[$(tput setaf 7)\]\A>\[$(tput setaf 1)\]\h:\[$(tput setaf 2)\]\W \\$ \[$(tput sgr0)\]"
# Prompt with git branch on the end
export PS1="\[$(tput bold)\]\[$(tput setaf 7)\]\A>\[$(tput setaf 1)\]\h:\[$(tput setaf 2)\]\W \$(parse_git_branch)\\$ \[$(tput sgr0)\]"

export CLICOLOR=1

export LSCOLORS=GxFxCxDxBxegedabagaced

# ln -s /tmp/.s.PGSQL.5432 /var/pgsql_socket/.s.PGSQL.5432
# ln -s /tmp/.s.PGSQL.5432.lock /var/pgsql_socket/.s.PGSQL.5432.lock 

# Show PWD in iTerm tabs
export PROMPT_COMMAND='echo -ne "\033]0;${PWD/#$HOME/~}\007"'

function parse_git_branch {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(on \1) /'
}
