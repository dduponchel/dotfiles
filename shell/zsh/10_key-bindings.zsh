# vi is too hardcore for a base config
bindkey -e

[[ -n ${key[Delete]} ]]         && bindkey "${key[Delete]}"         delete-char
[[ ${key[Control-Right]} != ${key[Right]} ]] && \
[[ -n ${key[Control-Right]} ]]  && bindkey "${key[Control-Right]}"  forward-word
[[ ${key[Control-Left]} != ${key[Left]} ]] && \
[[ -n ${key[Control-Left]} ]]   && bindkey "${key[Control-Left]}"   backward-word
[[ -n ${key[Home]} ]]           && bindkey "${key[Home]}"           beginning-of-line
[[ -n ${key[End]} ]]            && bindkey "${key[End]}"            end-of-line

# tips : bindkey '\e<char>' bind escape <char>

#bindkey '^r' history-incremental-search-backward # already present
[[ -n ${key[PageUp]} ]]         && bindkey "${key[PageUp]}"         up-line-or-history
[[ -n ${key[PageDown]} ]]       && bindkey "${key[PageDown]}"       down-line-or-history

# make search up and down work, so partially type and hit up/down to find relevant stuff
[[ -n ${key[Up]} ]]             && bindkey "${key[Up]}"             up-line-or-search
[[ -n ${key[Down]} ]]           && bindkey "${key[Down]}"           down-line-or-search

bindkey ' ' magic-space    # also do history expansion on space
