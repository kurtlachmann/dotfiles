## Windows setup
Treesitter requires a compiler (gcc, make) for building its parsers.
See: https://github.com/nvim-treesitter/nvim-treesitter/wiki/Windows-support

It didn't work with cygwin64. It only worked with mingw64.
Go to https://www.mingw-w64.org/downloads/ and download "MinGW-W64-builds".
For example from https://github.com/niXman/mingw-builds-binaries/releases
Unpack the build somewhere, e.g. to C:/mingw64/
Add the "bin" directory to your $PATH on neovim start:
```
vim.env.PATH = vim.env.PATH .. ";C:\\mingw64\\bin"
```
