

## Set up

### Mac OS:
  1. `ssh-keygen`
  1. Install
    - brew
    - iterm
    - Ukulele
  1. `brew install asdf coreutils direnv findutils gnu-sed grep jq kanata oh-my-posh wezterm`
  1. `brew install --cask --no-quarantine emacs`
  1. Install oh-my-zsh in .local/lib/oh-my-zsh
  1. Add paths to a file in  /etc/paths.d:
     - /usr/local/bin
     - /opt/homebrew/bin
  1. Install ~/.config/ukulele/Dvorak Custom for ALL USERS
  1. Clone asdf

### Main steps

  1. `sudo apt install git`
  2. `git clone --bare https://github.com/tjarvstrand/.cfg.git $HOME/.config/cfg`
  3. `alias config='/usr/bin/git --git-dir=$HOME/.config/cfg/ --work-tree=$HOME'`
  4. `config checkout -f`
  6. `config config --local status.showUntrackedFiles no`
  7. `config submodule update --init`
  8. `source ~/.bashrc`
  9. `config remote set-url origin git@github.com:tjarvstrand/.cfg.git`
  10. `config config remote.origin.fetch '+refs/heads/*:refs/remotes/origin/*'`

### Linux

  10. TODO: Add this to ansible
    - rm -r $HOME/Desktop && ln -s $HOME/Dropbox/Desktop $HOME/Desktop
    - rm -r $HOME/Documents && ln -s $HOME/Dropbox/Documents $HOME/Documents
    - rm -r $HOME/Pictures && ln -s $HOME/Dropbox/Pictures $HOME/Pictures

