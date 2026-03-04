autoload -Uz compinit
compinit

export CARAPACE_BRIDGES='zsh,fish,bash,inshellisense'
source <(carapace _carapace)

zstyle ':completion:*' format $'\e[2;37mCompleting %d\e[m'
zstyle ':completion:*:git:*' group-order 'main commands' 'alias commands' 'external commands'


