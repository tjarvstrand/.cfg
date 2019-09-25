# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

export EMAIL="tjarvstrand@gmail.com"
# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ]; then
    PATH="$HOME/.local/bin:$PATH"
fi

if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
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
export PATH=$PATH:${HOME}/.npm/bin


# Bazel ------------------------------------------------------------------------
if [ -f "${HOME}/.bazel/bin/bazel-complete.bash" ]
then
    source "${HOME}/.bazel/bin/bazel-complete.bash"
fi

export PATH="$HOME/.cargo/bin:$PATH"

if [ -f "${HOME}/.profile.local" ]
then
    source "${HOME}/.profile.local"
fi

if [ -f "${HOME}/.profile.$(hostname)" ]
then
    source "${HOME}/.profile.$(hostname)"
fi

# Riak -------------------------------------------------------------------------
ulimit -n 65536

