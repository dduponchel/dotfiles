
findFileInFpath() {
  local wantedFile=${1?must specify a file to search}
  IFS=:
  for zshPath in ${=FPATH}
  do
    [ -f "$zshPath/$wantedFile" ] && echo "$zshPath/$wantedFile" && return
  done
}

\#(){
  echo "comment ignored"
}
