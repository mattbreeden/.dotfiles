export EDITOR='vim'
mkcd() {
        if [ $# != 1 ]; then
                echo "Usage: mkcd <dir>"
        else
                mkdir -p $1 && cd $1
        fi
}

bind -r '\C-s'
stty -ixon

alias l='ls -A'
alias lah='ls -lah'
alias lf='ls -CF'

alias cd..='cd ..'
alias ..='cd ..'
alias cls='clear'

# git aliases
alias gs='git status'
alias gb='git branch'
alias ga='git add'
alias gc='git commit -m '
#git h is set up in .gitconfig
alias gh='git h'

# Tmux
alias tmux='tmux -2'
alias tks='tmux kill-session'
alias tls='tmux ls'

#Rubymotion
alias rr='reattach-to-user-namespace -l bundle exec rake'  

#Add RVM to PATH for scripting
PATH=$PATH:/usr/local/rvm/bin
### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"
#for Homebrew
export PATH="/usr/local/bin:$PATH"


# BEGIN Ruboto setup
source ~/.rubotorc
# END Ruboto setup
