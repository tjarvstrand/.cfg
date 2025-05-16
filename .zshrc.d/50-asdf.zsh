(( ! $+commands[asdf] )) && return

export ASDF_DATA_DIR="${HOME}/.cache/asdf"
path=("$ASDF_DATA_DIR/shims" $path)

if [ ! -f "$ZSH_COMPLETIONS_DIR/_asdf" ]
then
    asdf completion zsh > "$ZSH_COMPLETIONS_DIR/_asdf"
fi

export JAVA_OPTS="-Xss4m"
if which asdf > /dev/null && asdf plugin list | grep -qs '^java' && asdf current java 2>&1
then
    export JAVA_HOME=$(asdf where java)
fi
