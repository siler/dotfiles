function prompt_char {
	git branch >/dev/null 2>/dev/null && echo '} ' && return
	echo '> '
}

PROMPT='%{$fg[green]%}%B%n%b%{$reset_color%} at %{$fg[yellow]%}%B%m%b%{$reset_color%} in %{$fg[blue]%}%B${PWD/#$HOME/~}%b%{$reset_color%}$(git_prompt_info) $(prompt_char)'

ZSH_THEME_GIT_PROMPT_PREFIX=" on %{$fg[magenta]%}%B"
ZSH_THEME_GIT_PROMPT_SUFFIX="%b%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[green]%}!"
ZSH_THEME_GIT_PROMPT_CLEAN=""
