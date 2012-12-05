if command_exists keychain
then
  # id_rsa and id_dsa if any
  [ -d ~/.ssh ] && find ~/.ssh/ -name "id_*" -not -name "*pub" -exec keychain '{}' +
  [ -f ~/.keychain/$HOST-sh ] && source ~/.keychain/$HOST-sh
fi
