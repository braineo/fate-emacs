[![Build Status](https://travis-ci.com/braineo/fate-emacs.svg?branch=master)](https://travis-ci.com/braineo/fate-emacs)

## Get Started

Only tested with Emacs 26 in Debian and macOS

### Backup

backup your `.emacs` and `.emacs.d`, rename them to something else because emacs loads `.emacs` first if it exists


### Install

``` shell
git clone https://github.com/braineo/fate-emacs.git ~/.emacs.d
```

## Recommend tools

### ripgrep

ripgrep is basically a fast grep written in Rust.

Download ripgrep release from https://github.com/BurntSushi/ripgrep/releases

``` shell
wget https://github.com/BurntSushi/ripgrep/releases/download/0.10.0/ripgrep_0.10.0_amd64.deb && dpkg -i ripgrep_0.10.0_amd64.deb
```

for masOS

``` shell
brew install ripgrep
```

### python language server

``` shell
pip install python-language-server[all]
```

Or you can use Microsoft Python Lanugage Server

1. Install [dotnet-sdk](https://www.microsoft.com/net/download)
2. Clone and install [python-language-server](https://github.com/Microsoft/python-language-server)

``` shell
git clone https://github.com/Microsoft/python-language-server.git
cd python-language-server/src/LanguageServer/Impl
dotnet build -c Release
dotnet publish -c Release -r linux-x64 # linux
dotnet publish -c Release -r osx-x64   # mac
```

change the value of the `-r` flag depending on your architecture and
operating system.  See Microsoft's [Runtime ID Catalog](https://docs.microsoft.com/en-us/dotnet/core/rid-catalog) for the right
value for your system.

Then, link the executable to somewhere on your path, e.g.

``` shell
ln -sf $(git rev-parse --show-toplevel)/output/bin/Release/linux-x64/publish/Microsoft.Python.LanguageServer ~/.local/bin/
ln -sf $(git rev-parse --show-toplevel)/output/bin/Release/osx-x64/publish/Microsoft.Python.LanguageServer ~/.local/bin/
```

Credit to [lsp-python-ms](https://github.com/emacs-lsp/lsp-python-ms/blob/master/README.org)

## Optional Configs

Optional configuration, take a look at my [configs](https://github.com/braineo/configs) if interested

### EditorConfig (Code style)

``` shell
wget https://github.com/braineo/configs/blob/master/editorconfig/.editorconfig ~/.editorconfig
```

[More](https://editorconfig.org/)


### pycodestyle (python-language-server)

Exclude error codes and other configs

``` shell
mkdir -p ~/.config && wget https://github.com/braineo/configs/blob/master/pycodestyle/.config/pycodestyle ~/.config/pycodestyle
```

[Error codes](http://pycodestyle.pycqa.org/en/latest/intro.html#error-codes)

### all-the-icons

Install fonts for mode-line

`M-x` `all-the-icons-install-fonts`

## Useful links

* [How to change fonts](https://www.gnu.org/software/emacs/manual/html_node/emacs/Fonts.html)
* [Emacs documents](https://www.gnu.org/software/emacs/documentation.html)
