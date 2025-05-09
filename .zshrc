source "${HOME}/.profile"

# Path to your Oh My Zsh installation.
export ZSH="$HOME/.local/lib/oh-my-zsh"
export ZSH_CUSTOM="$HOME/.config/oh-my-zsh"

export SSH_AGENT_DISABLE="true"
export VIRTUAL_ENV_DISABLE_PROMPT="true"

# You may need to manually set your language environment
export LC_TYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8

autoload -U select-word-style
select-word-style bash

autoload -Uz compinit
compinit

for f in "$HOME"/.zshrc.d/*.zsh; do
    source $f
done

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time Oh My Zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="tjarvstrand"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
HYPHEN_INSENSITIVE="true"

# Uncomment one of the following lines to change the auto-update behavior
# zstyle ':omz:update' mode disabled  # disable automatic updates
# zstyle ':omz:update' mode auto      # update automatically without asking
# zstyle ':omz:update' mode reminder  # just remind me to update when it's time

# Uncomment the following line to change how often to auto-update (in days).
# zstyle ':omz:update' frequency 13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# You can also set it to another string to have that shown instead of the default red dots.
# e.g. COMPLETION_WAITING_DOTS="%F{yellow}waiting...%f"
# Caution: this setting can cause issues with multiline prompts in zsh < 5.7.1 (see #5765)
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
HIST_STAMPS="dd/mm/yyyy"

#zstyle :omz:plugins:ssh-agent lazy yes

setopt complete_aliases
setopt NO_BEEP NO_AUTOLIST BASH_AUTOLIST NO_MENUCOMPLETE NO_AUTO_MENU

plugins=(asdf direnv gcloud gitfast ssh-agent)

source $ZSH/oh-my-zsh.sh

eval "$(oh-my-posh init zsh --config ${HOME}/.zshrc.d/theme.omp.json)"

eval "$(atuin init zsh)"

