# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples
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
    (ps aux | grep bash | grep $(basename $f) > /dev/null) || rm -v ${f}
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

export GIT_SSH_COMMAND="ssh -q"

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi


# Git --------------------------------------------------------------------------
GIT_AUTHOR_NAME="Thomas Järvstrand"
GIT_COMMITTER_NAME="Thomas Järvstrand"

export WORK_EMAIL=""
export WORK_SRC_DIR=""
function cd_git {
  GIT_COMMITTER_EMAIL_ORIG=${GIT_COMMITTER_EMAIL}
  GIT_AUTHOR_EMAIL_ORIG=${GIT_AUTHOR_EMAIL}
  if [[ -n "${PWD}" && "${WORK_EMAIL}" != "" ]]; then
    if [[ "${WORK_SRC_DIR}" != "" && "$(readlink -f ${PWD})" == "${WORK_SRC_DIR}"* ]]; then
        GIT_COMMITTER_EMAIL_NEW=${WORK_EMAIL}
        GIT_AUTHOR_EMAIL_NEW=${WORK_EMAIL}
    else
        GIT_COMMITTER_EMAIL_NEW=${EMAIL}
        GIT_AUTHOR_EMAIL_NEW=${EMAIL}
    fi
    if [[ "${GIT_AUTHOR_EMAIL_NEW}" != "${GIT_AUTHOR_EMAIL}" ]]; then
        export GIT_COMMITTER_EMAIL=${GIT_COMMITTER_EMAIL_NEW}
        export GIT_AUTHOR_EMAIL=${GIT_AUTHOR_EMAIL_NEW}
        echo git email: ${GIT_AUTHOR_EMAIL_NEW}
    fi
  fi
}

function cd_otp {
  PATH_NO_OTP=$(echo $PATH |
                sed "s:${HOME}/.erlang.d/[^:]*::g" |
                tr -s ":")
  if [[ -n "${PWD}" ]]; then
    if [[ "$(readlink -f ${PWD})" == *"$HOME/klarna/fred"* ]]; then
        OTP_NEW="otp_17.5.6_kred"
    else
        if [[ "$(readlink -f ${PWD})" == *"$HOME/klarna/kred"* ]]; then
             OTP_NEW="18.3.4.5+kred1"
        else
        OTP_NEW="current"
        fi
    fi
    OTP_PATH_NEW="${HOME}/.erlang.d/${OTP_NEW}"
    if [[ "${OTP_NEW}" == "current" ]]
    then
        OTP_NAME=$(readlink ${OTP_PATH_NEW} | sed -r "s:.*/(.*)$:\1:")
        echo "OTP build: ${OTP_NAME} (current)"
    else
        echo "OTP build: ${OTP_NEW}"
    fi
    export PATH="${OTP_PATH_NEW}/bin:${PATH_NO_OTP}"
    # if [[ "${OTP_NEW}" != "${GIT_AUTHOR_EMAIL}" ]]; then
    #     export GIT_COMMITTER_EMAIL=${GIT_COMMITTER_EMAIL_NEW}
    #     export GIT_AUTHOR_EMAIL=${GIT_AUTHOR_EMAIL_NEW}
    #     echo git email: ${GIT_AUTHOR_EMAIL_NEW}
    # fi
  fi
}

function cd {
  builtin cd "${@:1}"
  cd_git
}
cd $PWD

function aws_adfs_prompt() {
  if [[ -n $AWS_SESSION_EXPIRATION_TIME ]] && [[ $AWS_SESSION_EXPIRATION_TIME -gt $(date +%s) ]]; then
    echo "[$AWS_PROFILE ($(( ($AWS_SESSION_EXPIRATION_TIME - $(date -u +%s)) / 60)) minute(s) remaining)]"
  fi
}

function aws-with-adfs-login
 {
    RES=$($(which aws-adfs-tool) login -r ${AWS_ADFS_ROLE} -a ${AWS_ADFS_ACCOUNT})
    RET=${?}
    if [[ "${RET}" == "0" ]]; then
        eval $(echo ${RES} | tee ${AWS_ENV})
    else
        echo ${RES}
        return ${RET}
    fi
}

export AWS_ENV=${HOME}/.aws/adfs-env
function aws-with-adfs {
    source ${AWS_ENV} 2>/dev/null
    EXPIRY=$(((${AWS_SESSION_EXPIRATION_TIME} - $(date -u +%s)) / 60))
    if [ ${EXPIRY} -le 0 ]; then
        aws-with-adfs-login
    fi
    EXPIRY=$(((${AWS_SESSION_EXPIRATION_TIME} - $(date -u +%s)) / 60))
    if [ $# -ne 0 ]; then
        $(which aws) ${@}
    fi
    >&2 echo "ADFS Session expires in ${EXPIRY} minutes"
}

export AWS_DEFAULT_REGION=eu-west-1

export AD_USERNAME=thojar
#alias aws='aws-with-adfs'
# export AWS_ADFS_ROLE=Klarna_ADFS_burrus
# export AWS_ADFS_ACCOUNT=${AWS_ADFS_ACCOUNT:-klarna-non-production}
# export AWS_DEFAULT_PROFILE=${AWS_ADFS_ROLE}@${AWS_ADFS_ACCOUNT}

function pr {
    stash pr create ${1:-${burrus}} ${@:2}
}

function otp {
    if [[ ! $1 ]]; then
        CURRENT=$(readlink "${HOME}/.erlang.d/current" | sed s:.*/::)
        OTP_VERSIONS=($(find ${HOME}/.erlang.d -maxdepth 1 -mindepth 1 -type d -printf %f\\n))
        COUNT=${#OTP_VERSIONS[@]}
        for i in $(seq 0 $((${COUNT} -1))); do
            OUT="$i. ${OTP_VERSIONS[${i}]}"
            if [[ "${CURRENT}" == "${OTP_VERSIONS[${i}]}" ]]; then
                DEFAULT=${i}
                OUT="\e[7m${OUT}\e[27m"
            fi
            echo -e "${OUT}"
        done
        read -p "Choose version ($DEFAULT): " VERSION
        if [[ ! ${VERSION} ]]; then
            VERSION=${DEFAULT}
        fi
        if [[ "${VERSION}" == "${DEFAULT}" ]]; then
            echo "Keeping OTP version ${OTP_VERSIONS[${VERSION}]}"
        else
            ln -sfvT ${HOME}/.erlang.d/${OTP_VERSIONS[${VERSION}]} ${HOME}/.erlang.d/current
            echo "Switched to OTP version ${OTP_VERSIONS[${VERSION}]}"
        fi
    fi
}

if [ -f "${HOME}/.bashrc.local" ]
then
    source "${HOME}/.bashrc.local"
fi

if [ -f "${HOME}/.bashrc.$(hostname)" ]
then
    source "${HOME}/.bashrc.$(hostname)"
fi

if [ -f "/home/tjarvstrand/.sdkman/bin/sdkman-init.sh" ]
then
    #THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
    export SDKMAN_DIR="/home/tjarvstrand/.sdkman"
    source "/home/tjarvstrand/.sdkman/bin/sdkman-init.sh"
fi

tmux-session

