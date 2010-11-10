autoload colors; colors;

# Enable colors
type dircolors > /dev/null 2>&1 && eval $(dircolors -b)
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

# Human-readable
alias df='df -h'
alias du='du -h'

# get the name of the branch we are on
git_prompt_info() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo "$ZSH_THEME_GIT_PROMPT_PREFIX${ref#refs/heads/}$(parse_git_dirty)$ZSH_THEME_GIT_PROMPT_SUFFIX"
}

parse_git_dirty () {
  if [[ -n $(git status -s 2> /dev/null) ]]; then
    echo "$ZSH_THEME_GIT_PROMPT_DIRTY"
  else
    echo "$ZSH_THEME_GIT_PROMPT_CLEAN"
  fi
}

# git theming default: Variables for theming the git info prompt
ZSH_THEME_GIT_PROMPT_PREFIX=" %{$fg[red]%}("     # Prefix at the very beginning of the prompt, before the branch name
ZSH_THEME_GIT_PROMPT_SUFFIX=")%{$reset_color%}"  # At the very end of the prompt
ZSH_THEME_GIT_PROMPT_DIRTY="*"                   # Text to display if the branch is dirty
ZSH_THEME_GIT_PROMPT_CLEAN=""                    # Text to display if the branch is clean

# If  set,  parameter  expansion, command substitution and arithmetic expansion are performed in
# prompts.  Substitutions within prompts do not affect the command status.
setopt prompt_subst

local prompt_previous_error prompt_date prompt_main_color prompt_user prompt_host prompt_path prompt_last_char

# if the last command didn't returned with 0, show the error
prompt_previous_error='%(?..%{$bg[red]%}%B<%?>%b%{$reset_color%} )'
prompt_date='%*'
# root (UID=0) = red, user = green
prompt_main_color='%(0#.%{$fg_bold[red]%}.%{$fg_bold[green]%})'
prompt_user='%n'
prompt_host='%m'
# a pwd with only the last two folders
prompt_path='%{$fg_bold[blue]%}%2~'
# # for root, $ for user
prompt_last_char='%(0#.#.$)'

PROMPT=${prompt_previous_error}${prompt_date}' '${prompt_main_color}${prompt_user}'@'${prompt_host}' '${prompt_path}'$(git_prompt_info)%{$reset_color%} '${prompt_last_char}' '

unset prompt_previous_error prompt_date prompt_main_color prompt_user prompt_host prompt_path prompt_last_char
