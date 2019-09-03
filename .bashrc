# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

LastRC=$?

deactivate 2>/dev/null

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
# HISTTIMEFORMAT='%F %T '

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

HISTFILE_DIR=${HOME}/.bash_hist
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

function aws_adfs_prompt() {
  if [[ -n $AWS_SESSION_EXPIRATION_TIME ]] && [[ $AWS_SESSION_EXPIRATION_TIME -gt $(date +%s) ]]; then
    echo "[$AWS_PROFILE ($(( ($AWS_SESSION_EXPIRATION_TIME - $(date -u +%s)) / 60)) minute(s) remaining)]"
  fi
}

source ${HOME}/bin/git-prompt

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
    DATE="$CYAN\D{%T %x}$RESETCOLOR"
    PS1="$RES [$DATE] ${GIT}\n\W \$ "
}

# If this is an xterm set the title to user@host:dir
PROMPT_COMMAND='set_prompt;update_hist'
#PROMPT_COMMAND='set_prompt'
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
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

export GIT_SSH_COMMAND="ssh -q"

function g () {
    find . -name "*.erl" -exec grep -rnH $@ {} \;
}



# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

export ERL_INETRC=${HOME}/.inetrc
export SSH_ENV="$HOME/.ssh/environment"
export LASTPASS_USERNAME="tjarvstrand@gmail.com"

# Paths ------------------------------------------------------------------------

if [[ -z $ORIG_PYTHONPATH ]]; then
   export ORIG_PYTHONPATH="${PYTHONPATH}"
fi
export PYTHONPATH=${ORIG_PYTHONPATH}


if [[ -z $ORIG_PATH ]]; then
  export ORIG_PATH="${PATH}"
fi
export PATH=${ORIG_PATH}

if [[ -z ${ORIG_MANPATH} ]]; then
    if [[ -z ${MANPATH} ]]; then
        export ORIG_MANPATH=$(manpath -q)
    else
        export ORIG_MANPATH=${MANPATH}
    fi
fi
export MANPATH=${ORIG_MANPATH}

export OTP_PATH="${HOME}/.erlang.d/current"
export PATH="${OTP_PATH}/bin:${PATH}"
export DIALYZER_PLT="${OTP_PATH}/dialyzer.plt"

# Misc paths
export PATH="${PATH}:${HOME}/.erlang.d/current/bin"

# Go
export GOROOT="/usr/local/lib/go"
export GOPATH="${HOME}/src/golang"
export PATH="${GOROOT}/bin:${GOPATH}/bin:${PATH}"

# Ansible ----------------------------------------------------------------------
export ANSIBLE_HOME=${HOME}/src/ansible
export PATH=${ANSIBLE_HOME}/bin:${PATH}
export MANPATH=${MANPATH}:${ANSIBLE_HOME}/docs/man
export PYTHONPATH=${ANSIBLE_HOME}/lib:${PYTHONPATH}

# Java -------------------------------------------------------------------------
export JAVA_HOME=$(readlink -f /usr/bin/javac | sed "s:bin/javac::")
export PATH=${PATH}:${JAVA_HOME}/bin

# Scala ------------------------------------------------------------------------
# SCALA_VERSION=2.11.11
# export SCALA_HOME=/usr/local/lib/scala-${SCALA_VERSION}
# export PATH=${PATH}:${SCALA_HOME}/bin

# Android ----------------------------------------------------------------------
export ANDROID_HOME=$HOME/Android/Sdk
export PATH=$PATH:$ANDROID_HOME/emulator
export PATH=$PATH:$ANDROID_HOME/tools
export PATH=$PATH:$ANDROID_HOME/tools/bin
export PATH=$PATH:$ANDROID_HOME/platform-tools
export JAVA_OPTS="-Xmx2G -XX:+UseConcMarkSweepGC -XX:+CMSClassUnloadingEnabled -Xss2M -Duser.timezone=GMT"

# npm --------------------------------------------------------------------------
#export PATH=$PATH:/usr/local/lib/node/current/bin
export PATH=$PATH:${HOME}/.npm/bin



# Bazel ------------------------------------------------------------------------
source "${HOME}/.bazel/bin/bazel-complete.bash"

# Riak -------------------------------------------------------------------------
ulimit -n 65536

# Python -----------------------------------------------------------------------
if [ -f "${HOME}/.virtualenv/bin/activate" ]
then
    . ${HOME}/.virtualenv/bin/activate
fi

# Git --------------------------------------------------------------------------
GIT_AUTHOR_NAME="Thomas Järvstrand"
GIT_COMMITTER_NAME="Thomas Järvstrand"

export WORK_EMAIL="tjarvstrand@contractor.ea.com"
WORK_SRC_DIR="$HOME/dice"
function cd_git {
  GIT_COMMITTER_EMAIL_ORIG=${GIT_COMMITTER_EMAIL}
  GIT_AUTHOR_EMAIL_ORIG=${GIT_AUTHOR_EMAIL}
  if [[ -n "${PWD}" ]]; then
    if [[ "$(readlink -f ${PWD})" == *"${WORK_SRC_DIR}"* ]]; then
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
  #cd_otp
}
cd $PWD

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

function tmux-session {
    if ! pidof tmux > /dev/null
    then
        exec tmux
    fi
    TMUX_SESSIONS=$(tmux list-sessions)
    TMUX_SESSION_COUNT=${#TMUX_SESSIONS[@]}
    echo default $DEFAULT_TMUX_SESSION
    for i in $(seq 0 $((${TMUX_SESSION_COUNT} -1))); do
        echo -e "${TMUX_SESSIONS[${i}]}"
    done
    read -p "Choose session [New]: " TMUX_SESSION
    if [[ -z "${TMUX_SESSION}" ]]; then
        exec tmux
    fi
    tmux attach -t ${TMUX_SESSION} || tmux
}

if [ -f "${HOME}/.bashrc.local" ]
then
    source "${HOME}/.bashrc.local"
fi

if [ "$NO_TMUX" == "" ] && which tmux > /dev/null && [[ -z "${TMUX}" ]]
then
    tmux-session
fi
