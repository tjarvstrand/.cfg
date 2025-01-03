if [[ $OSTYPE == 'darwin'* ]]; then
    mkdir -p $HOME/bin
    for c in {grep,readlink,sed}; do
        if [ ! -L "$HOME/bin/$c" ]; then
            ln -sv "/opt/homebrew/bin/g$c" "$HOME/bin/$c"
        fi
    done

    export ANDROID_HOME="$HOME/Library/Android/sdk"
    export PATH="/opt/homebrew/opt/postgresql@15/bin:$PATH"
fi
