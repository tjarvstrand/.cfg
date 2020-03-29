## Set up

  1. `sudo apt install git`
  2. `git clone --bare https://github.com/tjarvstrand/.cfg.git $HOME/.cfg`
  3. `alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'`
  4. `config checkout -f`
  5. `config config --local status.showUntrackedFiles no`
  6. `source ~/.bashrc`
