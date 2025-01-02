export PROMPT_COMMAND="echo -ne '\033]0;${USER}@${HOST}\007';$PROMPT_COMMAND"
precmd() { eval "$PROMPT_COMMAND" }

git_repo () {
    basename $(git rev-parse --show-toplevel 2> /dev/null) 2> /dev/null
}
setopt PROMPT_SUBST

# Last return code
PROMPT=$'%B%(?.%{$fg[green]%}\342\234\223.%{$fg[red]%}\342\234\227)%{$reset_color%} '

# Time
PROMPT+=$'[%{$fg[cyan]%}%D{%X}%{$reset_color%}] '

# Git
PROMPT+='%{$fg[magenta]%}$(git_repo)%{$reset_color%} $(git_prompt_info)'

# Current directory. Use $PWD, since  %~ doesn't expand `hash`.
PROMPT+=$'\n$(basename $PWD) $ '

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[green]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY=" %{$fg[red]%}*%{$fg[green]%}"
ZSH_THEME_GIT_PROMPT_CLEAN=""
