# Human-readable
alias df='df -h'
alias du='du -h'

command_exists rlwrap && alias telnet="rlwrap telnet $@"

alias :q="exit"
alias ss="sudo su -"


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
