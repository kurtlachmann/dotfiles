# dotfiles
My personal dotfiles

# Installation
Run the `install.sh` script to install dotfiles for the most basic tools.

```
$ git clone https://github.com/kurtlachmann/dotfiles.git ~/.dotfiles
$ cd ~/.dotfiles
$ ./install.sh
```

For other things like VSCode or Urxvt please use a manual installation:
Make sure you have [`stow`](https://www.gnu.org/software/stow/) installed.
Then to setup e.g. VSCode run:

```
$ stow vscode
```
