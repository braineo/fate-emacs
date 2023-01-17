[![Build Status](https://travis-ci.com/braineo/fate-emacs.svg?branch=master)](https://travis-ci.com/braineo/fate-emacs)
![GitHub](https://img.shields.io/github/license/braineo/fate-emacs)

  - [Get Started](#get-started)
      - [Backup](#backup)
      - [Install](#install)
      - [Try it in docker](#try-it-in-docker)
  - [External tools](#external-tools)
      - [ripgrep](#ripgrep)
      - [Tools configured for Python](#tools-configured-for-python)
      - [all-the-icons](#all-the-icons)
  - [Customize](#customize)
      - [Custom.el](#custom.el)
      - [EditorConfig (Code style)](#editorconfig-code-style)
  - [Language servers](#language-servers)
      - [MicroSoft Python Language Server](#microsoft-python-language-server)
  - [Useful links](#useful-links)



## Get Started

Only tested with \>= Emacs 26.3 in Debian and macOS

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

## External tools

### ripgrep

ripgrep is basically a fast grep written in Rust.

Download ripgrep release from https://github.com/BurntSushi/ripgrep/releases

``` shell
# example
wget https://github.com/BurntSushi/ripgrep/releases/download/0.10.0/ripgrep_0.10.0_amd64.deb && dpkg -i ripgrep_0.10.0_amd64.deb
# or
apt install ripgrep
```

for masOS

``` shell
brew install ripgrep
```

### fd

`fd` is a replacement for `find`

``` shell
sudo apt install fd-find
```


### Tools configured for Python

| Name            | Installation command        | Description                                                                         |
|-----------------|-----------------------------|-------------------------------------------------------------------------------------|
| flake8          | pip install flake8          | Flake8 is a wrapper around PyFlakes, pycodestyle and Ned Batchelder’s McCabe script |
| python-black    | pip install black           | Black is the uncompromising Python code formatter                                   |
| black-macchiato | pip install black-macchiato | Enable formatting of partial files using black                                      |

### Tools configured for Go

| Name    | Installation command                       | Description                                    |
|---------|--------------------------------------------|------------------------------------------------|
| gopls   | go install golang.org/x/tools/gopls@latest | Go language server                             |
| gofumpt | go install mvdan.cc/gofumpt@latest         | Go formater, sometimes works better than gopls |

### Tools configured for JavaScript/JSON

| Name     | Installation command | Description                                              |
|----------|----------------------|----------------------------------------------------------|
| prettier | npm i -g prettier    | Formater for JavaScript, TypeScript, CSS, JSON, and more |


### Tools configured for shell script


| Name       | Installation command                       | Description                                           |
|------------|--------------------------------------------|-------------------------------------------------------|
| shellcheck | apt install shellcheck                     | A great teacher helping you write better shell script |
| shfmt      | go install mvdan.cc/sh/v3/cmd/shfmt@latest | A shell script formater                               |


### all-the-icons

Install fonts for mode-line

`M-x` `all-the-icons-install-fonts`

## Customize

### Custom.el

The first time you open Emacs, it will create a `custom.el` for you automatically from `fate-custom-template.el`. There you can made custom configuration does not go with emacs for different computers (like your personal ones and working ones). For example, `PATH` variable.

Especially for macOS users, because `exec-path-from-shell` is slow that I did not include it and you might want to edit ENV variables and `exec-path`

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

#### Custom variable for packages

``` emacs-lisp
(setq lsp-python-ms-extra-paths
  '("/path/to/a/site-packages"
    "/path/to/b/site-packages"))
```

### EditorConfig (Code style)

EditorConfig helps maintain consistent coding styles for multiple developers working on the same project across various editors and IDEs. The EditorConfig project consists of a file format for defining coding styles and a collection of text editor plugins that enable editors to read the file format and adhere to defined styles. EditorConfig files are easily readable and they work nicely with version control systems.

``` shell
wget https://github.com/braineo/configs/blob/master/editorconfig/.editorconfig ~/.editorconfig
```

Optional configuration, take a look at my [configs](https://github.com/braineo/configs) if interested

[More](https://editorconfig.org/)

## Install and Update Language Servers

Lsp has a good website documenting usage of [lsp-mode](https://emacs-lsp.github.io/lsp-mode/)

### Language servers implemented in NodeJS

Language servers implemented in NodeJS can obtain directly by doing `lsp-install-server`. For updating installed servers do `C-0` then `lsp-install-server`.

| Language              | Installation command                                     |
| --------------------- | -------------------------------------------------------- |
| TypeScript/JavaScript | npm i -g typescript-language-server; npm i -g typescript |
| JSON                  | npm i -g vscode-json-languageserver                      |
| CSS/LessCSS/SASS/SCSS | npm install -g vscode-css-languageserver-bin             |
| HTML                  | npm install -g vscode-html-languageserver-bin            |
| Dockerfile            | npm install -g dockerfile-language-server-nodejs         |

### Go

Install `gopls`

``` shell
GO111MODULE=on go get gopls
```

### Python (MicroSoft Python Language Server)

`lsp-python-ms` now downloads language server binary automatically, or you can build latest Microsoft Python Lanugage Server by yourself

1.  Install [dotnet-sdk](https://www.microsoft.com/net/download)
2.  Clone and install [python-language-server](https://github.com/Microsoft/python-language-server)

<!-- end list -->

``` shell
git clone https://github.com/Microsoft/python-language-server.git
cd python-language-server/src/LanguageServer/Impl
dotnet build -c Release
dotnet publish -c Release -r linux-x64 # linux
dotnet publish -c Release -r osx-x64   # mac
```

change the value of the `-r` flag depending on your architecture and operating system. See Microsoft’s [Runtime ID Catalog](https://docs.microsoft.com/en-us/dotnet/core/rid-catalog) for the right value for your system.

Then, link the executable to somewhere on your path, e.g.

``` shell
ln -sf $(git rev-parse --show-toplevel)/output/bin/Release/linux-x64/publish/Microsoft.Python.LanguageServer ~/.emacs.d/mspyls/
ln -sf $(git rev-parse --show-toplevel)/output/bin/Release/osx-x64/publish/Microsoft.Python.LanguageServer ~/.emacs.d/mspyls/
```

Credit to [lsp-python-ms](https://github.com/emacs-lsp/lsp-python-ms/blob/master/README.org)

Because Microsoft’s Python language server does not ship with a linter, need to install flake8

``` shell
pip install flake8
```

``` emacs-lisp
;; Configure extra search path
(setq lsp-python-ms-extra-paths
  '("path1"
    "path2"))

;; If you need to work on some python2 projects, make sure use a right flake8
(setq flycheck-python-flake8-executable "python2")
```

### Clangd

`clangd` needs `compile_commands.json` to know the compiler flags that are used to build the project, when some custom paths are used.

For cmake project, passing `-DCMAKE_EXPORT_COMPILE_COMMANDS=ON` to `cmake` can generate `compile_commands.json`. If it is not a cmake project,
[bear](https://github.com/rizsotto/Bear) is a right tool to generate it.

## Vterm

To make vterm handy, you need some shell-side configuration, add following to `.zshrc` or `.bashrc`

``` shell
if [[ "$INSIDE_EMACS" = 'vterm' ]] \
    && [[ -n ${EMACS_VTERM_PATH} ]] \
    && [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh ]]; then
	source ${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh
fi
```


## Useful links

  - [How to change fonts](https://www.gnu.org/software/emacs/manual/html_node/emacs/Fonts.html)
  - [Emacs documents](https://www.gnu.org/software/emacs/documentation.html)
