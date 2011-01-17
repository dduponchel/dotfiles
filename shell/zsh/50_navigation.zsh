alias d='dirs -v'

cd() {
  builtin cd "$@" && ls
}

take() {
  mkdir -p $1
  cd $1
}

# List direcory contents
alias l='ls -la'
alias ll='ls -l'
alias sl=ls # often screw this up

# If a command is issued that can't be executed as a normal command, and the command is the name
# of a directory, perform the cd command to that directory.
setopt auto_cd

# what with this behavior ? cd <tab> autocompletes users...
# If the argument to a cd command (or an implied cd with the AUTO_CD option set) is not a direc-
# tory,  and does not begin with a slash, try to expand the expression as if it were preceded by
# a `~' (see the section `Filename Expansion').
setopt NO_cdable_vars

# Make cd push the old directory onto the directory stack.
setopt auto_pushd

# Don't push multiple copies of the same directory onto the directory stack.
setopt pushd_ignore_dups

