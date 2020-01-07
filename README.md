[![Build Status](https://travis-ci.com/braineo/fate-emacs.svg?branch=master)](https://travis-ci.com/braineo/fate-emacs)

## Get Started

Only tested with > Emacs 26.2 in Debian and macOS

### Backup

backup your `.emacs` and `.emacs.d`, rename them to something else because emacs loads `.emacs` first if it exists


### Install

``` shell
git clone https://github.com/braineo/fate-emacs.git ~/.emacs.d
```

### Try it in docker

``` shell
docker pull braineo/fate-emacs
# run in terminal
docker run -it --rm braineo/fate-emacs
# run with GUI
xhost +local:root # WARN: this comes with security issues
docker run -it --rm -e DISPLAY -v /tmp/.X11-unix:/tmp/.X11-unix -v /your/workspace/:/mnt/workspace braineo/fate-emacs
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

### Language servers

#### MicroSoft Python Language Server

`lsp-python-ms` now downloads language server binary automatically, or you can build latest Microsoft Python Lanugage Server by yourself

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
ln -sf $(git rev-parse --show-toplevel)/output/bin/Release/linux-x64/publish/Microsoft.Python.LanguageServer ~/.emacs.d/mspyls/
ln -sf $(git rev-parse --show-toplevel)/output/bin/Release/osx-x64/publish/Microsoft.Python.LanguageServer ~/.emacs.d/mspyls/
```
Credit to [lsp-python-ms](https://github.com/emacs-lsp/lsp-python-ms/blob/master/README.org)

Because MicroSoft's Python language server does not ship with a linter, need to install flake8

``` shell
pip install flake8
```

| Language              | Installation command                                     |
|-----------------------|----------------------------------------------------------|
| TypeScript/JavaScript | npm i -g typescript-language-server; npm i -g typescript |
| JSON                  | npm i -g vscode-json-languageserver                      |
| CSS/LessCSS/SASS/SCSS | npm install -g vscode-css-languageserver-bin             |
| HTML                  | npm install -g vscode-html-languageserver-bin            |
| Dockerfile            | npm install -g dockerfile-language-server-nodejs         |


### all-the-icons

Install fonts for mode-line

`M-x` `all-the-icons-install-fonts`

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

### Adding your custom parameters

The first time you open Emacs, it will create a `custom.el` for you automatically from `fate-custom-template.el`. There you can made custom configuration does not go with emacs for different computers (like your personal ones and working ones). For example, font face/size and executable path

#### exec-path

``` emacs-lisp
(setq exec-path (append exec-path '("/path/to/bin")))
```

#### Setting ENV variables

``` emacs-lisp
(setenv "LD_LIBRARY_PATH"
  (let ((current (getenv "LD_LIBRARY_PATH"))
        (new "/path/to/lib/"))
    (if current (concat new ":" current) new)))
```

#### custom variable for packages

``` emacs-lisp
(setq lsp-python-ms-extra-paths
  '("/path/to/a/site-packages"
    "/path/to/b/site-packages"))
```





## Useful links

* [How to change fonts](https://www.gnu.org/software/emacs/manual/html_node/emacs/Fonts.html)
* [Emacs documents](https://www.gnu.org/software/emacs/documentation.html)
