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

export GREP_OPTIONS='--color=auto'
#export GREP_COLOR='1;32'
