
osx () {

    alias config='/usr/bin/git --git-dir=$HOME/.config/cfg.mac/ --work-tree=$HOME'

if which asdf > /dev/null
then
    export ASDF_DATA_DIR="${HOME}/.cache/asdf"
    source "/usr/local/lib/asdf.sh"
    export PATH="$ASDF_DATA_DIR/shims:$PATH"
fi

source '/Library/Developer/CommandLineTools/usr/share/git-core/git-completion.bash'

}

[[ $OSTYPE == 'darwin'* ]] && osx
