
osx () {

    if which asdf > /dev/null
    then
        export ASDF_DATA_DIR="${HOME}/.cache/asdf"
        source "/usr/local/lib/asdf.sh"
        export PATH="$ASDF_DATA_DIR/shims:$PATH"
    fi

    source '/Library/Developer/CommandLineTools/usr/share/git-core/git-completion.bash'

    mkdir -p $HOME/bin
    for c in {grep,readlink,sed}; do
        if [ ! -L "$HOME/bin/$c" ]; then
        x    ln -sv "/opt/homebrew/bin/g$c" "$HOME/bin/$c"
        fi
    done

    export PATH="$PATH:/opt/homebrew/bin"
    for f in /opt/homebrew/etc/bash_completion.d/*; do
        . $f
    done

}

[[ $OSTYPE == 'darwin'* ]] && osx
