#!/usr/bin/env bash 

ncursesInclude="/nix/store/ffjl7aw7f0gjwv4gb4mgb0w49v5dhrkg-ncurses-6.0-20171125-dev/include/"
ncursesLib="/nix/store/54cwjh1lsmjpk2cbs43gw89w4zhk3ybb-ncurses-6.0-20171125/lib/"
#g++ main.cpp -I$ncursesInclude -L$ncursesLib -lncurses -o main

HISTCONTROL=ignoredups
set -o emacs 

alias n='nvim'
alias games='nixGL steam'
alias c='clear'
alias nixconfig='sudo nvim /etc/nixos/configuration.nix'
alias switch='sudo nixos-rebuild switch'
alias ni='nix-env -iA'
alias bashu='source ~/.bashrc'
alias l='ls'
alias la='ls -a'
alias p='cd ~/Progs/PycharmProjects/CDW/'
alias d='cd ~/Downloads'
alias dot='cd ~/Dotfiles/.config'
alias ..='cd ..'
alias b='btop'
alias ls='exa'
alias la='exa -alh'
alias tree='exa  --tree'
alias ovpn='sudo openvpn --config /home/username/Dotfiles/.config/agrankov.ovpn &'

alias wireguard_on='sudo herd start wireguard-wg0'
alias wireguard_off='sudo herd stop wireguard-wg0'
alias ubuntu='sudo qemu-system-x86_64 -enable-kvm -cpu host,kvm=on,+hypervisor,+invtsc,l3-cache=on,migratable=no,hv_passthrough -cdrom /home/username/VMs/ubuntu/ubuntu-22.04.3-desktop-amd64.iso -drive file=/home/username/VMs/ubuntu/disk.img -smp cores=2,threads=2,sockets=1 -m 5G -boot menu=on -vga virtio -display sdl,gl=on'
fish
