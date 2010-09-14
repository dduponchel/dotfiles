export SVN_EDITOR=vim
export EDITOR=vim
export BROWSER=firefox

export PAGER="less -FRX"

export PATH=~/.bin:~/bin:$PATH

# a tmux within a tmux within a tmux ...
[[ -n "$TMUX" ]] && unset TMUX

[[ -z "$DISPLAY" ]] && export DISPLAY=:0
