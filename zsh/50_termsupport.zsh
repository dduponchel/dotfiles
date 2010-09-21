if [ -n "$DISPLAY" ]
then
  preexec () {
    print -Pn "\e]0;%n@%m: $1\a"
  }
  precmd () {
    print -Pn "\e]0;%n@%m: %~\a"
  }
fi
