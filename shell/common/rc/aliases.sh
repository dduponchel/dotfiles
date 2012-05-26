# Enable colors
command_exists dircolors && eval $(dircolors -b)
if [ "$(uname|grep Linux)" ]
then
  alias ls='ls --color=auto -ph'
  alias tree='tree -CF'
elif [ "$(uname|grep BSD)" ]
then
  alias ls='ls -FGh'
  alias tree='tree -CF'
fi

# less handles colors :)
alias less='less -R'

# Human-readable
alias df='df -h'
alias du='du -h'

# Show history
alias history='fc -l 1'

alias notepad++='wine "C:\Program Files\Notepad++\notepad++.exe"'
command_exists rlwrap && alias telnet="rlwrap telnet $@"

alias :q="exit"
alias ss="sudo su -"


function down4me() {
  curl -s "http://www.downforeveryoneorjustme.com/$1" | sed '/just you/!d;s/<[^>]*>//g'
}

function getip {
  ifconfig | grep "inet " | awk '{ print $2 }'
  res=$(curl -s checkip.dyndns.org | grep -Eo '[0-9\.]+')
  echo "Public IP : $(curl -s checkip.dyndns.org | grep -Eo '[0-9\.]+')"
}


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

# firefox's addon sdk
alias addon-sdk="cd /opt/addon-sdk && source bin/activate; cd -"
