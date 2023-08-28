
osx () {
    mkdir -p $HOME/bin
    for c in {grep,readlink,sed}; do
        if [ ! -L "$HOME/bin/$c" ]; then
            ln -sv "/opt/homebrew/bin/g$c" "$HOME/bin/$c"
        fi
    done

    export PATH="/opt/homebrew/bin:$PATH"
    source /opt/homebrew/etc/bash_completion

    export ANDROID_HOME="$HOME/Library/Android/sdk"

}

[[ $OSTYPE == 'darwin'* ]] && osx
