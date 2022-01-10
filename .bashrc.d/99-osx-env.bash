osx () {
    env | while IFS= read -r line; do
        value=${line#*=}
        name=${line%%=*}
        if [ "$name" != "_" ]; then
            launchctl setenv "$name" "$value"
        fi
    done
}

[[ $OSTYPE == 'darwin'* ]] && osx
