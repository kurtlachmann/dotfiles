#!/bin/bash

stow bash -v
stow zsh -v
stow vim -v
stow tmux-2.9 -v

ln -i -s $(pwd)/nvim ~/.config/nvim

utils/install_vim_plugins.sh
utils/install_zsh_plugins.sh
