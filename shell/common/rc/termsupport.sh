if command_exists infocmp && ! infocmp &>/dev/null
then
  echo "WARN : this term ($TERM) is unknown, falling back to xterm"
  export TERM=xterm
fi

if [ -n "$DISPLAY" ]
then
  preexec () {
    print -Pn "\e]0;%n@%m: $1\a"
  }
  precmd () {
    print -Pn "\e]0;%n@%m: %~\a"
  }
fi
