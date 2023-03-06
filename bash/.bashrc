#!/usr/bin/env bash 
ncursesInclude="/nix/store/ffjl7aw7f0gjwv4gb4mgb0w49v5dhrkg-ncurses-6.0-20171125-dev/include/"
ncursesLib="/nix/store/54cwjh1lsmjpk2cbs43gw89w4zhk3ybb-ncurses-6.0-20171125/lib/"
#g++ main.cpp -I$ncursesInclude -L$ncursesLib -lncurses -o main

HISTCONTROL=ignoredups
set -o vi

alias n='nvim'
alias games='nixGL steam'
alias c='clear'
alias nixconfig='sudo nvim /etc/nixos/configuration.nix'
alias switch='sudo nixos-rebuild switch'
alias ni='nix-env -iA'
alias bashu='source ~/.bashrc'
alias l='ls'
alias la='ls -a'
alias p='cd ~/proga_vuz/'


export PATH="/usr/local/share/rsi/idl/bin:/home/username/.local/bin:/home/username/Documents/Win_Apps/pico-8/:$PATH"

