

## Set up

### Mac OS:
  1. `ssh-keygen`
  1. Install
    - brew
    - Ukulele
  1. `brew install coreutils direnv findutils gnu-sed grep jq oh-my-posh wezterm`emacs
  1. Add /opt/homebrew/bin to the top of /etc/paths
  1. Install ~/.config/ukulele/Dvorak Custom (should be automatic now?)

### Linux
1. `sudo apt install git`
1. TODO: Add this to ansible
    - rm -r $HOME/Desktop && ln -s $HOME/Dropbox/Desktop $HOME/Desktop
    - rm -r $HOME/Documents && ln -s $HOME/Dropbox/Documents $HOME/Documents
    - rm -r $HOME/Pictures && ln -s $HOME/Dropbox/Pictures $HOME/Picturesx

### Common steps
  1. `git clone --bare https://github.com/tjarvstrand/.cfg.git $HOME/.config/cfg`
  1. `alias config='/usr/bin/git --git-dir=$HOME/.config/cfg/ --work-tree=$HOME'`
  1. `config checkout -f`
  1. `config config --local status.showUntrackedFiles no`
  1. `config submodule update --init`
  1. `source ~/.bashrc`
  1. `config remote set-url origin git@github.com:tjarvstrand/.cfg.git`
  1. `config config remote.origin.fetch '+refs/heads/*:refs/remotes/origin/*'`

