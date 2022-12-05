
osx () {
    source '/Library/Developer/CommandLineTools/usr/share/git-core/git-completion.bash'

    mkdir -p $HOME/bin
    for c in {grep,readlink,sed}; do
        if [ ! -L "$HOME/bin/$c" ]; then
            ln -sv "/opt/homebrew/bin/g$c" "$HOME/bin/$c"
        fi
    done

    export PATH="/opt/homebrew/bin:$PATH"
    for f in /opt/homebrew/etc/bash_completion.d/*; do
        . $f
    done

    export ANDROID_HOME="$HOME/Library/Android/sdk"

}

[[ $OSTYPE == 'darwin'* ]] && osx
