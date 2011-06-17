## smart urls
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

# List jobs in the long format by default.
setopt long_list_jobs

# if user and system execution times > Xs, display timing stats
REPORTTIME=10
# use this format to report execution times
TIMEFMT="$bold_color%J$reset_color took ${fg[red]}%E$reset_color. ${fg[yellow]}%U$reset_color user, ${fg[yellow]}%S$reset_color system, ${fg[yellow]}%P$reset_color cpu"

alias notepad++='wine "C:\Program Files\Notepad++\notepad++.exe"'
command_exists rlwrap && alias telnet="rlwrap telnet $@"

alias :q="exit"
alias vim="vim -p"
alias ss="sudo su -"


function down4me() {
  curl -s "http://www.downforeveryoneorjustme.com/$1" | sed '/just you/!d;s/<[^>]*>//g'
}

function getip {
  ifconfig | grep "inet " | awk '{ print $2 }'
  res=$(curl -s checkip.dyndns.org | grep -Eo '[0-9\.]+')
  echo "Public IP : $(curl -s checkip.dyndns.org | grep -Eo '[0-9\.]+')"
}
