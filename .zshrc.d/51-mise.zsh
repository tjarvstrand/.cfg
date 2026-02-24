if which mise >/dev/null; then
    eval "$(mise activate zsh)"
    mise completion zsh > "$ZSH_COMPLETIONS_DIR/_mise"
fi
