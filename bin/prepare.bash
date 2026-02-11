#!/usr/bin/env bash

set -e

if command -v cargo >/dev/null 2>&1; then
    echo "Installing tools with cargo"
    cargo install ripgrep fd-find typos-cli difftastic tealdeer git-delta ast-grep stylua taplo-cli
fi

if command -v npm >/dev/null 2>&1; then
    echo "Configure npm to install to ~/.local"
    npm config set prefix "$HOME/.local/"
    echo "installing language servers"
    npm install -g prettier @typescript/native-preview vscode-langservers-extracted bash-language-server yaml-language-server graphql-language-service-cli
fi

if command -v go >/dev/null 2>&1; then
    go install mvdan.cc/sh/v3/cmd/shfmt@latest
fi


if command -v apt >/dev/null 2>&1; then
    echo "install packages for jinx and lsp bridge"
    sudo apt install libenchant-2-dev pkg-config python3-venv pipx pandoc fonts-jetbrains-mono
fi

echo "install packages for python"
if command -v pipx >/dev/null 2>&1; then
    pipx ensurepath
    pipx install ruff-lsp
    pipx install ruff
fi
