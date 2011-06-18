## smart urls
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

# List jobs in the long format by default.
setopt long_list_jobs

# if user and system execution times > Xs, display timing stats
REPORTTIME=10
# use this format to report execution times
TIMEFMT="$bold_color%J$reset_color took ${fg[red]}%E$reset_color. ${fg[yellow]}%U$reset_color user, ${fg[yellow]}%S$reset_color system, ${fg[yellow]}%P$reset_color cpu"

