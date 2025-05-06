# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

export LANG=en_US.UTF-8
export LC_TYPE=en_US.UTF-8
export LC_CTYPE=UTF-8
export LC_ALL=en_US.UTF-8
export XDG_CONFIG_HOME="$HOME/.config"

export CLICOLOR=1

export EMAIL="tjarvstrand@gmail.com"
export ERL_INETRC=${HOME}/.inetrc
export SSH_ENV="$HOME/.ssh/environment"

export EDITOR="emacs --no-site-file --no-splash -nw --debug-init -q --eval '(setq basic-setup t)' -l ~/.emacs"

PROP="libinput Natural Scrolling Enabled"

if which xinput > /dev/null; then
  for id in $(xinput --list |
                  grep -E 'slave\s+pointer' |
                  grep -Eo 'id=[0-9]+' |
                  cut -d'=' -f 2); do
      if xinput list-props $id |
              grep -qi "$PROP"; then
        xinput set-prop $id "$PROP" 1
    fi
  done
fi

# Paths ------------------------------------------------------------------------

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

if [ -d "$HOME/.local/bin" ]; then
    export PATH="$HOME/.local/bin:$PATH"
fi

if [ -d "$HOME/bin" ] ; then
    export PATH="$HOME/bin:$PATH"
fi

if [[ -z $ORIG_PYTHONPATH ]]; then
   export ORIG_PYTHONPATH="${PYTHONPATH}"
fi
export PYTHONPATH=${ORIG_PYTHONPATH}

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
JAVA_HOME_SCRIPT="$HOME/.cache/asdf/plugins/java/set-java-home.bash"
if [ -f "$JAVA_HOME_SCRIPT" ]; then
    bash "$JAVA_HOME_SCRIPT"
    export PATH=${PATH}:${JAVA_HOME}/bin
elif [ -f /usr/bin/javac ]; then
    export JAVA_HOME=$(readlink -f /usr/bin/javac | sed "s:/bin/javac::")
    export PATH=${PATH}:${JAVA_HOME}/bin
fi

# Scala ------------------------------------------------------------------------
# SCALA_VERSION=2.11.11
# export SCALA_HOME=/usr/local/lib/scala-${SCALA_VERSION}
# export PATH=${PATH}:${SCALA_HOME}/bin

# Android ----------------------------------------------------------------------
if [ -d "$HOME/Android/Sdk" ]; then
    export ANDROID_HOME=$HOME/Android/Sdk
elif [ -d "$HOME/Library/Android/sdk/" ]; then
    export ANDROID_HOME="$HOME/Library/Android/sdk"
fi
if [ -n "$ANDROID_HOME" ]; then
    export PATH=$PATH:$ANDROID_HOME/emulator:$ANDROID_HOME/tools:$ANDROID_HOME/tools/bin:$ANDROID_HOME/platform-tools
fi
export JAVA_OPTS="-Xmx2G -XX:+CMSClassUnloadingEnabled -Xss2M -Duser.timezone=GMT"

# npm --------------------------------------------------------------------------
export PATH=$PATH:${HOME}/.npm/bin


# Bazel ------------------------------------------------------------------------
if [ -f "${HOME}/.bazel/bin/bazel-complete.bash" ]
then
    source "${HOME}/.bazel/bin/bazel-complete.bash"
fi

# Dart -------------------------------------------------------------------------
export PATH="$PATH":"$HOME/.pub-cache/bin"



# Riak -------------------------------------------------------------------------
ulimit -n 65536

# Other ------------------------------------------------------------------------

if [ -f "${HOME}/.profile.local" ]
then
    source "${HOME}/.profile.local"
fi

if [ -f "${HOME}/.profile.$(hostname)" ]
then
    source "${HOME}/.profile.$(hostname)"
fi

