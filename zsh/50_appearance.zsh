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
function git_prompt_info() {
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

PROMPT='%(?..%{$bg[red]%}%B<%?>%b%{$reset_color%} )%* %(0#.%{$fg_bold[red]%}.%{$fg_bold[green]%})%n@%m %{$fg_bold[blue]%}%2~$(git_prompt_info)%{$reset_color%} %(0#.#.$) '