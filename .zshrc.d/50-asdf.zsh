if [ -d "${HOME}/.local/lib/asdf" ]
then
    export ASDF_DATA_DIR="${HOME}/.cache/asdf"
    source "${HOME}/.local/lib/asdf/asdf.sh"
    fpath=(${ASDF_DIR}/completions $fpath)
fi

export JAVA_OPTS="-Xss4m"
if which asdf > /dev/null && asdf plugin list | grep -qs '^java' && asdf current java 2>&1
then
    export JAVA_HOME=$(asdf where java)
fi
