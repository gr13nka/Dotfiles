sudo apt update
sudo apt install rofi exa tree fzf git stow vim neovim i3 polybar kitty fish compton feh openvpn wireguard qutebrowser syncthing language-pack-ru
snap install obsidian --classic
rm ~/.bashrc 
cd ~/Dotfiles
stow .
fish

#add ru lang
sudo update-locale LANG=ru_RU.UTF-8
done
#install qmk
sudo apt install -y git python3-pip
python3 -m pip install --user qmk
qmk setup
qmk config user.keyboard=crkbd/rev1
qmk config user.keymap=gr13nka

#last neovim
sudo add-apt-repository ppa:neovim-ppa/unstable -y
sudo apt update
sudo apt upgrade

