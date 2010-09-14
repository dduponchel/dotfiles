## smart urls
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

## file rename magick
bindkey "^[m" copy-prev-shell-word

# List jobs in the long format by default.
setopt long_list_jobs


alias notepad++='wine "C:\Program Files\Notepad++\notepad++.exe"'
alias lock='xscreensaver-command -lock'
[ -x /usr/bin/rlwrap ] && alias telnet="rlwrap telnet $@"

alias :q="exit"
alias vim="vim -p"
alias ss="sudo su -"
