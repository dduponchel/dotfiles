export SVN_EDITOR=vim
export EDITOR=vim
export BROWSER=firefox

export PAGER="less -FRX"

export GREP_OPTIONS='--color=auto'
#export GREP_COLOR='1;32'

export PATH=~/.bin:~/bin:~/.cabal/bin:~/.local/bin:$PATH

# see man bash
export BASH_ENV=~/.bash_profile

# a tmux within a tmux within a tmux ...
[[ -n "$TMUX" ]] && unset TMUX
