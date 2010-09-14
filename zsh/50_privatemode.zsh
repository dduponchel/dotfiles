function privatemode() {
	if [ -z "$PRIVATE_MODE" ]
	then
			# go !
			export PROMPTOLD=$PROMPT
			export PROMPT="%{$fg_bold[white]%}[[%{$fg_bold[magenta]%}PRIVATE%{$fg_bold[white]%}]]%{$reset_color%} $PROMPT"

			export PRIVATE_MODE="1"
	else
			# normal mode
			export PROMPT=$PROMPTOLD
			unset PROMPTOLD

			unset PRIVATE_MODE
	fi
}

# thanks hist_ignore_space
alias privatemode=" privatemode"

# add to history only if PRIVATE_MODE is empty
zshaddhistory () {
		[[ -z $PRIVATE_MODE ]]
}

