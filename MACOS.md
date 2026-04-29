# Mac specific installation guide

## Configure jinx

Install necessary packages

```shell
brew install enchant pkgconfig hunspell
```

Then install dictionary for hunspell. Download dictionaries from http://wordlist.aspell.net/dicts/ and put them to `~/Library/Spelling/` or `/Library/Spelling/`.

Check if hunspell can load dictionaries by

```shell
hunspell -D
```

By default Emacs launched in macOS uses locale `C.UTF-8`, that bypasses `LANG` env var for jinx, you need to configure explicitly using:

```lisp
(set-locale-environment "en_US.UTF-8")
```
