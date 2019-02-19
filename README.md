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


## Useful links

* [How to change fonts](https://www.gnu.org/software/emacs/manual/html_node/emacs/Fonts.html)
* [Emacs documents](https://www.gnu.org/software/emacs/documentation.html)
