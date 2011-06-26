# check if a command exists
command_exists() {
  type $1 > /dev/null 2>&1
}
