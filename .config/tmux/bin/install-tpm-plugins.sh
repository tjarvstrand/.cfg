#!/usr/bin/env bash

CURRENT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TPM_DIR="$CURRENT_DIR/../plugins/tpm"

source "$TPM_DIR/scripts/helpers/plugin_functions.sh"

for plugin in "$(tpm_plugins_list_helper)"; do
    IFS='#' read -ra plugin <<< "$plugin"
    if ! plugin_already_installed "$plugin"; then
        install='true'
    fi
done

if [ -n "$install" ]; then
    exec "$TPM_DIR/scripts/install_plugins.sh"
fi
