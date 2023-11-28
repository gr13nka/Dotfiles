# Set up Guix Home profile
#if [ -f ~/.profile ]; then . ~/.profile; fi

# Honor per-interactive-shell startup file
if [ -f ~/.bashrc ]; then . ~/.bashrc; fi

# Merge search-paths from multiple profiles, the order matters.
#eval "$(guix package --search-paths \
#-p $HOME/.config/guix/current \
#-p $HOME/.guix-profile \
#-p /run/current-system/profile)"

# Prepend setuid programs.
export PATH=/run/setuid-programs:$PATH
export PATH=~/.local/bin/ruff-lsp:$PATH
export PATH=/home/username/.local/bin:$PATH
export PATH=/var/lib/flatpak/exports/share:$PATH
export PATH=/home/username/.local/share/flatpak/exports/share:$PATH
export PATH=/home/username/pycharm-community-2023.2.4/bin:$PATH

