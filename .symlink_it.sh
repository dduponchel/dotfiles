#!/usr/bin/env bash

function relink() {
  rm -ri $1
  ln -sn $2 $1
}

cd `dirname $0`
DOTFILES=$PWD

cd

relink .bin              $DOTFILES/bin
relink .conkyrc          $DOTFILES/conkyrc
relink .conkystatusbarrc $DOTFILES/conkystatusbarrc
relink .dzen             $DOTFILES/dzen
relink .mplayer          $DOTFILES/mplayer
relink .vimrc            $DOTFILES/vimrc
relink .xinitrc          $DOTFILES/xinitrc
relink .Xresources       $DOTFILES/Xresources
relink .zsh              $DOTFILES/zsh
relink .zprofile         $DOTFILES/zprofile
# for bash compatibility, and xinitrc (see comments)
relink .profile          $DOTFILES/zprofile
relink .zshrc            $DOTFILES/zshrc

mkdir -p .xmonad
relink .xmonad/xmonad.hs $DOTFILES/xmonad.hs

# quodlibet plugins : not the ideal way to handle my changes...
mkdir -p .quodlibet/plugins/{editing,songsmenu}/
relink .quodlibet/plugins/editing/iconv.py      $DOTFILES/quodlibet/plugins/editing/iconv.py
relink .quodlibet/plugins/songsmenu/openwith.py $DOTFILES/quodlibet/plugins/songsmenu/openwith.py
