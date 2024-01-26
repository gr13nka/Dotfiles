fish_add_path /home/username/.local/bin
fish_add_path /var/lib/flatpak/exports/share
fish_add_path /home/username/pycharm-community-2023.2.4/bin
fish_add_path /home/username/.local/share/flatpak/exports/share

fish_add_path /home/username/.cargo/bin

set fish_greeting

if status is-interactive
    # Commands to run in interactive sessions can go here
end


alias n="nvim"
alias games="nixGL steam"
alias c="clear"
alias bashu="source ~/.bashrc"
alias l="ls"
alias la="ls -a"
alias p="cd ~/Progs/PycharmProjects/CDW/"
alias d="cd ~/Downloads"
alias dot="cd ~/Dotfiles/.config"
alias ..="cd .."
alias b="btop"
alias ls="exa"
alias make_writable="sudo chmod o+rw /dev/ttyACM0"
alias la="exa -alh"

alias tree="exa  --tree"
alias siz="du -h"
alias ovpn="sudo openvpn --config /home/username/Dotfiles/.config/agrankov.ovpn &"

alias wireguard_on="sudo herd start wireguard-wg0"
alias wireguard_off="sudo herd stop wireguard-wg0"
#alias ubuntu="sudo qemu-system-x86_64 -enable-kvm -cpu host,kvm=on,+hypervisor,+invtsc,l3-cache=on,migratable=no,hv_passthrough -cdrom /home/username/VMs/ubuntu/ubuntu-22.04.3-desktop-amd64.iso -drive file=/home/username/VMs/ubuntu/disk.img -smp cores=2,threads=2,sockets=1 -m 5G -boot menu=on -vga virtio -display sdl,gl=on"

alias ubuntu="sudo qemu-system-x86_64 -enable-kvm -cpu host,kvm=on,+hypervisor,+invtsc,l3-cache=on,migratable=no,hv_passthrough -drive file=/home/username/VMs/ubuntu/disk.img -smp cores=2,threads=2,sockets=1 -m 5G -boot menu=on -vga virtio -display sdl,gl=on"
