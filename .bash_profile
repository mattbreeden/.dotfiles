[[ -s ~/.bashrc ]] && source ~/.bashrc
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

# Custom bash prompt via kirsle.net/wizards/ps1.html
# export PS1="\[$(tput bold)\]\[$(tput setaf 7)\]\A>\[$(tput setaf 1)\]\h:\[$(tput setaf 2)\]\W \\$ \[$(tput sgr0)\]"
# Space added to the end of ~/.rvm/bin/rvm-prompt line:
# then gemset="${rvm_gemset_separator:-"@"}${ruby_string##*${rvm_gemset_separator:-"@"}} "

export PS1="\[$(tput bold)\]\[$(tput setaf 7)\]\A>\[$(tput setaf 1)\]\h:\[$(tput setaf 2)\]\W \$(parse_git_branch)\\[$(tput setaf 4)\]\$(~/.rvm/bin/rvm-prompt g)\[$(tput setaf 2)\]\$ \[$(tput sgr0)\]"

 #\[$(tput setaf 4)\]\$(~/.rvm/bin/rvm-prompt g)
export CLICOLOR=1

export LSCOLORS=GxFxCxDxBxegedabagaced

# Show PWD in iTerm tabs
export PROMPT_COMMAND='echo -ne "\033]0;${PWD/#$HOME/~}\007"'

function parse_git_branch {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(on \1) /'
}
