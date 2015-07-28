[[ -s ~/.bashrc ]] && source ~/.bashrc
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

BASE16_SHELL="$HOME/.config/base16-shell/base16-tomorrow.dark.sh"
[[ -s $BASE16_SHELL ]] && source $BASE16_SHELL

# Custom bash prompt via kirsle.net/wizards/ps1.html
# export PS1="\[$(tput bold)\]\[$(tput setaf 7)\]\A>\[$(tput setaf 1)\]\h:\[$(tput setaf 2)\]\W \\$ \[$(tput sgr0)\]"
# Prompt with git branch on the end
export PS1="\[$(tput setaf 7)\]\A\[$(tput setaf 1)\]❯\[$(tput setaf 3)\]❯\[$(tput setaf 2)\]❯ \[$(tput setaf 1)\]\h:\[$(tput setaf 2)\]\W \$(parse_git_branch)\\[$(tput setaf 4)\]\$(~/.rvm/bin/rvm-prompt g)\[$(tput setaf 2)\]$ \[$(tput sgr0)\]"
 #\[$(tput setaf 4)\]\$(~/.rvm/bin/rvm-prompt g)

export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagaced

# Show PWD in iTerm tabs
export PROMPT_COMMAND='echo -ne "\033]0;${PWD/#$HOME/~}\007"'

function parse_git_branch {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1) /'
}

[[ -s ~/.profile ]] && source ~/.profile
