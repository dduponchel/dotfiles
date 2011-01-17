if command_exists keychain
then
  # id_rsa and id_dsa if any
  [ -d ~/.ssh ] && find ~/.ssh/ -name "id_?sa" -exec keychain '{}' +
  [ -f ~/.keychain/$HOST-sh ] && source ~/.keychain/$HOST-sh
fi

# annoying bug since v4.3.3, commit 7072c10ae223e24f601b3
autoload -U is-at-least
is-at-least 4.3.3 || return
# correction (will surely break things, but will correct mine :p)

local originalSshFile=$(findFileInFpath _ssh)

if [ ! -z $originalSshFile ]
then
  local sshFile=${ZSH:-~}/._ssh$$
  sed -r 's/\&\& return//' $originalSshFile > $sshFile
  source $sshFile
  rm -f $sshFile
  unset sshFile
fi

unset originalSshFile
