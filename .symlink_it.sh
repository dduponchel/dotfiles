#!/usr/bin/env bash

# readlink -f ? forget it, -f isn't a valid option on BSD.
cd `dirname $0`
DOTFILES=$PWD

DESTDIR=${1:-~}
[ ! -d $DESTDIR ] && echo "$DESTDIR is not a folder" 1>&2 && exit 1

# relink config_file [dest]
# If dest is not specified, dest = ~/.${config_file}
# remove dest if possible and create a symlink from config_file to dest.
function relink() {
  config_file=$DOTFILES/$1
  dest=${2:-$DESTDIR/.$1}

  # a broken symlink isn't a file...
  [ -e $dest -o -L $dest ] && rm -ri $dest
  ln -sn $config_file $dest
}

relink bin
relink conkyrc
relink conkystatusbarrc
relink dzen-icons
relink mplayer
mkdir -p $DESTDIR/.vim
relink vim-plugins $DESTDIR/.vim/bundle
relink vim-pathogen/autoload $DESTDIR/.vim/autoload
relink vimrc
relink gitconfig
relink xinitrc
relink Xresources
relink zsh
relink profile $DESTDIR/.zprofile
# for bash compatibility, and xinitrc (see comments)
relink profile $DESTDIR/.bash_profile
relink bashrc
relink zshrc
relink npmrc

mkdir -p $DESTDIR/.xmonad
relink xmonad.hs $DESTDIR/.xmonad/xmonad.hs

# quodlibet plugins : not the ideal way to handle my changes...
mkdir -p $DESTDIR/.quodlibet/plugins/{editing,songsmenu}/
relink quodlibet/plugins/editing/iconv.py
relink quodlibet/plugins/songsmenu/openwith.py
