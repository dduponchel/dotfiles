# Ignore completion functions for commands you don't have
zstyle ':completion:*:functions' ignored-patterns '_*'

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
# ps -o user:15,comm doesn't work on BSD, but I don't need it : BSD's ps is smart enough to
# display my full login...
local ps_command
if [ "$(uname|grep Linux)" ]
then
	ps_command="ps -U `whoami` -o pid,user:15,comm -w -w"
elif [ "$(uname|grep BSD)" ]
then
	ps_command="ps -U `whoami` -o pid,user,comm -w -w"
fi
zstyle ':completion:*:*:*:*:processes' command $ps_command
unset ps_command

# With commands like rm, it's annoying if you keep getting offered the same
# file multiple times. This fixes it. Also good for cp, et cetera..
zstyle ':completion:*:rm:*' ignore-line yes
zstyle ':completion:*:cp:*' ignore-line yes
zstyle ':completion:*:mv:*' ignore-line yes

# If this option is unset, output flow control via start/stop  characters  (usually  assigned  to
# ^S/^Q) is disabled in the shell's editor.
#setopt NO_flow_control

# `WORDCHARS'
# 	A list of non-alphanumeric characters considered part of a word by
# 	the line editor.
# I want / to delimit words
WORDCHARS="${WORDCHARS:s#/#}"

# The following lines were added by compinstall

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
zstyle ':completion:*' completions 1
zstyle ':completion:*' expand prefix suffix
zstyle ':completion:*' file-sort name
zstyle ':completion:*:descriptions' format '%B---- %d%b'
zstyle ':completion:*:messages'     format '%B%U---- %d:%u%b'
zstyle ':completion:*:warnings'     format '%B%U---- no match in %d%u%b'
zstyle ':completion:*' glob 1
zstyle ':completion:*' group-name ''
zstyle ':completion:*' ignore-parents parent pwd
zstyle ':completion:*' insert-unambiguous false
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' matcher-list 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' max-errors 2
zstyle ':completion:*' menu select=1 yes
zstyle ':completion:*' original false
zstyle ':completion:*' prompt '%e error(s) found '
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true

autoload -Uz compinit
compinit

# End of lines added by compinstall

# zsh-lovers
rationalise-dot() {
  if [[ $LBUFFER = *.. ]]; then
    LBUFFER+=/..
  else
    LBUFFER+=.
  fi
}
zle -N rationalise-dot
bindkey . rationalise-dot
