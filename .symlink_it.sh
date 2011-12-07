#!/usr/bin/env bash

# readlink -f ? forget it, -f isn't a valid option on BSD.
DOTFILES=$( (cd -P $(dirname $0) && pwd) )

C_SEPARATOR="\033[1;34;40m"
C_CLEAR="\033[1;0m"

DESTDIR=${1:-~}
[ ! -d $DESTDIR ] && echo "$DESTDIR is not a folder" 1>&2 && exit 1
cd $DESTDIR

# relink config_file [dest]
# If dest is not specified, dest = ~/.${config_file without path}
# remove dest if possible and create a symlink from config_file to dest.
function relink() {
  config_file=$DOTFILES/$1
  dest=${2:-.${1##*/}}
  dest_dir=`dirname $dest`

  [[ ! -d $dest_dir ]] && mkdir -p $dest_dir
  printf "${dest} ${C_SEPARATOR}:: ${C_CLEAR}"
  if [ -L "$dest" ]
  then
    printf "symbolic link, updating\n"
    rm -f "$dest"
    # a broken symlink isn't a file...
  elif [ -e "$dest" ]
  then
    printf "file, "
    rm -ri $dest
  else
    printf "not found, creating\n"
  fi
  ln -sn $config_file $dest
}

relink bin

relink mplayer

relink git/gitconfig
relink git/gitignore

relink npm/npmrc

relink x/conkyrc
relink x/xinitrc
relink x/Xresources

relink {,.}xmonad/xmonad.hs
relink xmonad/conkystatusbarrc
relink xmonad/dzen-icons

relink vim/plugins .vim/bundle
relink vim/pathogen/autoload .vim/autoload
relink vim/vimrc
mkdir -p .vim/tmp/{undo,backup,swap}

relink shell/zsh
relink shell/profile .zprofile
# for bash compatibility, and xinitrc (see comments)
relink shell/profile .bash_profile
relink shell/bashrc
relink shell/zshrc
relink shell/common .shell

relink ackrc
relink tmux.conf

# quodlibet plugins : not the ideal way to handle my changes...
relink {,.}quodlibet/plugins/editing/iconv.py
relink {,.}quodlibet/plugins/songsmenu/openwith.py

# TODO find why bash doesn't expand "for profile in .mozilla/firefox/*/prefs.js"
# with no match (should do 0 loop)
for profile in `ls .mozilla/firefox/*/prefs.js 2> /dev/null`
do
  prefs=${profile%prefs.js}user.js
  relink firefox/user.js $prefs
done
