## smart urls
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

# List jobs in the long format by default.
setopt long_list_jobs


alias notepad++='wine "C:\Program Files\Notepad++\notepad++.exe"'
command_exists rlwrap && alias telnet="rlwrap telnet $@"

alias :q="exit"
alias vim="vim -p"
alias ss="sudo su -"
