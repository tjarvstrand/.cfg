# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

source "${HOME}/.profile"

export LC_TYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8

export EDITOR="emacs --no-site-file --no-splash -nw --debug-init -q --eval '(setq basic-setup t)' -l ~/.emacs"

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoredups:ignorespace

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=10000
HISTFILESIZE=10000
HISTIGNORE='ls:bg:fg:history'

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

HISTFILE_DIR=${HOME}/.cache/bash_history
mkdir -p ${HISTFILE_DIR}
HISTFILE=${HISTFILE_DIR}/${BASHPID}
GLOBAL_HISTFILE=${HISTFILE_DIR}/global
function update_hist() {
    cmd="$(history | tail -n 1 | sed 's/^[ 0-9]*//'| sed 's/\\/\\\\/g')"
    awk='$0 != cmd {print $0} END {print cmd}'

    touch ${HISTFILE}
    awk -v cmd="${cmd}" "${awk}" ${HISTFILE} > ${HISTFILE}.tmp
    mv ${HISTFILE}.tmp ${HISTFILE}

    touch ${GLOBAL_HISTFILE}
    awk -v cmd="${cmd}" "${awk}" ${GLOBAL_HISTFILE} > ${GLOBAL_HISTFILE}.tmp
    mv ${GLOBAL_HISTFILE}.tmp ${GLOBAL_HISTFILE}

    # Local history will be present in both local and global history, but local
    # history will be first so that's ok for now.
    history -c
    history -r ${GLOBAL_HISTFILE}
    history -r ${HISTFILE}
}

for f in $(find ${HISTFILE_DIR} -name '[0-9]*');
do
    (ps -p $(basename $f) > /dev/null) || (echo -n "Removing " && rm -v ${f})
done

history -c
history -r ${GLOBAL_HISTFILE}
history -r ${HISTFILE}

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

source ${HOME}/.local/bin/git-prompt

set_prompt () {
    LASTRC=$?
    RESETCOLOR='\e[m'
    CYAN="\[\033[0;36m\]"
    MAGENTA="\[\033[0;35m\]"
    RED="\[\033[1;31m\]"
    YELLOW="\[\033[0;33m\]"
    BLUE="\[\033[34m\]"
    GREEN="\[\033[1;32m\]"

    GIT_PS1_SHOWDIRTYSTATE=true
    GIT_PS1_SHOWUPSTREAM=''
    GIT=$(__git_ps1 "%s")

    if [[ "$GIT" =~ \*$ ]]; then
        GIT_COLOR=$RED # Unstaged changes
    elif [[ "$GIT" =~ \+$ ]]; then
        GIT_COLOR=$YELLOW # Staged, uncommitted changes
    else
        GIT_COLOR=$GREEN # Clean state
    fi
    GIT="$GIT_COLOR$GIT$RESETCOLOR"

    if [[ $LASTRC == 0 ]]; then
        RES="$GREEN\342\234\223$RESETCOLOR"
    else
        RES="$RED\342\234\227$RESETCOLOR"
    fi
    GIT_REPO=$(basename $(git rev-parse --show-toplevel 2> /dev/null) 2> /dev/null)
    if [ -n "${GIT_REPO}" ]
    then
        GIT_REPO="${MAGENTA}${GIT_REPO}${RESET_COLOR}"
    fi
    DATE="$CYAN\D{%T %x}$RESETCOLOR"
    PS1="$RES [$DATE] ${GIT_REPO} ${GIT}\n\W \$ "
}

# If this is an xterm set the title to user@host:dir
PROMPT_COMMAND='set_prompt;update_hist'
case "$TERM" in
xterm*|rxvt*)
    PROMPT_COMMAND=$PROMPT_COMMAND'; echo -ne "\033]0;${USER}@${HOSTNAME}\007"'
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls -h --color=auto'
else
    alias ls='ls -h'
fi

alias rc='source ${HOME}/.bashrc'
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias gw='./gradlew'
alias grep='grep --color=auto'
alias config='/usr/bin/git --git-dir=$HOME/.config/cfg/ --work-tree=$HOME'
alias copy='xclip -i -sel clip'
alias dc='docker-compose'
alias a='asdf'
alias curl='curl -sS'

export GIT_SSH_COMMAND="ssh -q"

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    source /etc/bash_completion
fi
if [ -f  "${HOME}/.local/lib/ansible/completion.bash" ]
then
    source "${HOME}/.local/lib/ansible/completion.bash"
fi


# Git --------------------------------------------------------------------------
GIT_AUTHOR_NAME="Thomas Järvstrand"
GIT_COMMITTER_NAME="$GIT_AUTHOR_NAME"

# Other -----------------------------------------------------------------------

function jq_less {
    $(which jq) -rC $@ | less -FR
}

alias jq=jq_less

deactivate 2>/dev/null
if [ -f "${HOME}/.virtualenv/bin/activate" ]
then
    . ${HOME}/.virtualenv/bin/activate
fi

if which direnv > /dev/null; then
    eval "$(direnv hook bash)"
fi

# Strip trailing semicolon
export PROMPT_COMMAND="${PROMPT_COMMAND%;}"

for f in "$HOME"/.bashrc.d/*; do
    source $f
done

tmux-session

