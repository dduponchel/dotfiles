# Try to correct the spelling of all arguments in a line.
setopt correct_all

alias man='nocorrect man'
alias mv='nocorrect mv'
alias mkdir='nocorrect mkdir'

# The spelling prompt
SPROMPT='correction '$fg[red]%R%{$reset_color%}' => '$fg[green]%r%{$reset_color%}' ? ([Y]es/[N]o/[E]dit/[A]bort) '
