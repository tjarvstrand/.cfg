source "${HOME}/.profile"

export ZSH_COMPLETIONS_DIR="$HOME/.config/zsh/completions"
mkdir -p "$ZSH_COMPLETIONS_DIR"
fpath=("$ZSH_COMPLETIONS_DIR" $fpath)

# # You may need to manually set your language environment
export LC_TYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8

autoload -U select-word-style
select-word-style bash

for f in "$HOME"/.zshrc.d/*.zsh; do
    source $f
done

setopt complete_aliases
setopt NO_BEEP NO_AUTOLIST BASH_AUTOLIST NO_MENUCOMPLETE NO_AUTO_MENU

export VIRTUAL_ENV_DISABLE_PROMPT="true"
eval "$(oh-my-posh init zsh --config ${HOME}/.zshrc.d/theme.omp.json)"

