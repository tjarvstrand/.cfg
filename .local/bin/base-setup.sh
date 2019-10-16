
# Run this with eval "$(wget -q -O - https://raw.githubusercontent.com/tjarvstrand/.cfg/master/bin/base-setup.sh)"

sudo apt update
sudo apt install git python3 python3-pip
sudo pip3 install ansible

DIR="$HOME/.config/cfg"
if [ ! -d "$DIR" ]
then
    mkdir -p $(dirname $DIR)
    echo $DIR >> $HOME/.gitignore
    git clone --bare https://github.com/tjarvstrand/.cfg.git $DIR

    rm $HOME/.gitignore
    rm $HOME/.bashrc
    rm $HOME/.profile
fi

CONFIG="/usr/bin/git --git-dir=$DIR --work-tree=$HOME"

$CONFIG checkout
$CONFIG config status.showUntrackedFiles no
alias config="$CONFIG"
