

## Set up

### Mac OS prep:
  1. `ssh-keygen`
  1. Install
    - brew
    - iterm
    - Ukulele
  1. `brew install bash asdf grep coreutils findutils gnu-sed direnv`
  1. `brew install --cask --no-quarantine emacs`
  1. `chsh -s /usr/local/bin/bash`

### Main steps

  1. `sudo apt install git`
  2. `git clone https://github.com/tjarvstrand/.cfg.git $HOME/.config/cfg`
  2. `git clone --bare https://github.com/tjarvstrand/.cfg.mac.git $HOME/.config/cfg.mac`
  3. `alias config='/usr/bin/git --git-dir=$HOME/.config/cfg.mac/ --work-tree=$HOME'`
  4. `config checkout -f`
  6. `config config --local status.showUntrackedFiles no`
  7. `source ~/.bashrc`
  8. `config remote set-url origin git@github.com:tjarvstrand/.cfg.mac.git`
  9. `config config remote.origin.fetch '+refs/heads/*:refs/remotes/origin/*'`

### Linux

  10. TODO: Add this to ansible
    - rm -r $HOME/Desktop && ln -s $HOME/Dropbox/Desktop $HOME/Desktop
    - rm -r $HOME/Documents && ln -s $HOME/Dropbox/Documents $HOME/Documents
    - rm -r $HOME/Pictures && ln -s $HOME/Dropbox/Pictures $HOME/Pictures

