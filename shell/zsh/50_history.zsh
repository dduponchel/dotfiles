## Command history configuration
HISTFILE=$HOME/.zsh_history
HISTSIZE=11000 # larger, see HIST_EXPIRE_DUPS_FIRST
SAVEHIST=10000
# Show history
alias history='fc -l 1'

# Do not enter command lines into the history list if they are duplicates of the previous event.
setopt hist_ignore_dups

# This  option  both imports new commands from the history file, and also causes your typed com-
# mands to be appended to the history file (the latter is like  specifying  INC_APPEND_HISTORY).
# The  history lines are also output with timestamps ala EXTENDED_HISTORY (which makes it easier
# to find the spot where we left off reading the file after it gets re-written).
setopt share_history

# Whenever  the  user  enters  a  line  with history expansion, don't execute the line directly;
# instead, perform history expansion and reload the line into the editing buffer.
setopt hist_verify

# If the internal history needs to be trimmed to add the  current  command  line,  setting  this
# option  will  cause  the  oldest history event that has a duplicate to be lost before losing a
# unique event from the list.  You should be sure to set the value of HISTSIZE to a larger  num-
# ber  than  SAVEHIST  in  order to give you some room for the duplicated events, otherwise this
# option will behave just like HIST_IGNORE_ALL_DUPS  once  the  history  fills  up  with  unique
# events.
setopt hist_expire_dups_first

# Remove command lines from the history list when the first character on the line is a space, or
# when one of the expanded aliases contains a leading space.  Note that the command  lingers  in
# the  internal  history  until  the next command is entered before it vanishes, allowing you to
# briefly reuse or edit the line.  If you want to make it vanish  right  away  without  entering
# another command, type a space and press return.
setopt hist_ignore_space

# If this is set, zsh sessions will append their history list to the history file,  rather  than
# replace it. Thus, multiple parallel zsh sessions will all have the new entries from their his-
# tory lists added to the history file, in the order that they exit.  The  file  will  still  be
# periodically  re-written to trim it when the number of lines grows 20% beyond the value speci-
# fied by $SAVEHIST (see also the HIST_SAVE_BY_COPY option).
setopt append_history
