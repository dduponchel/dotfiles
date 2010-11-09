#!/usr/bin/env bash

# readlink -f ? forget it, -f isn't a valid option on BSD.
cd `dirname $0`
DOTFILES=$PWD

# relink config_file [dest]
# If dest is not specified, dest = ~/.${config_file}
# remove dest if possible and create a symlink from config_file to dest.
function relink() {
  config_file=$DOTFILES/$1
  dest=${2:-~/.$1}

  [ -e $dest ] && rm -ri $dest
  ln -sn $config_file $dest
}

relink bin
relink conkyrc
relink conkystatusbarrc
relink dzen
relink mplayer
relink vim
relink vim-pathogen/autoload ~/.vim/autoload
relink vimrc
relink xinitrc
relink Xresources
relink zsh
relink profile ~/.zprofile
# for bash compatibility, and xinitrc (see comments)
relink profile ~/.bash_profile
relink bashrc
relink zshrc

mkdir -p .xmonad
relink xmonad.hs ~/.xmonad/xmonad.hs

# quodlibet plugins : not the ideal way to handle my changes...
mkdir -p .quodlibet/plugins/{editing,songsmenu}/
relink quodlibet/plugins/editing/iconv.py
relink quodlibet/plugins/songsmenu/openwith.py
