## Set up

  1. `sudo apt install git`
  2. `git clone --bare https://github.com/tjarvstrand/.cfg.git $HOME/.config/cfg`
  3. `alias config='/usr/bin/git --git-dir=$HOME/.config/cfg/ --work-tree=$HOME'`
  4. `config checkout -f`
  5. `config config --local status.showUntrackedFiles no`
  6. `source ~/.bashrc`
  7. `config remote set-url origin git@github.com:tjarvstrand/.cfg.git`
