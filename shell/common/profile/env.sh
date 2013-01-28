export SVN_EDITOR=vim
export EDITOR=vim
export BROWSER=firefox

export PAGER="less -FRX"

export PATH=~/.dotfiles/external/dmenu:~/.bin:~/.cabal/bin:~/.local/bin:$PATH

# see man bash
export BASH_ENV=~/.bash_profile

# a tmux within a tmux within a tmux ...
[[ -n "$TMUX" ]] && unset TMUX

# Hey GTK : use X Input Method !
# see http://canonical.org/~kragen/setting-up-keyboard.html
export GTK_IM_MODULE=xim

# to get sound in some steam games
export SDL_AUDIODRIVER=alsa
