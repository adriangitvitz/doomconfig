## Doom emacs Keratoconus

### How to install Theme

Clone kera theme by using sparse checkout

``` shell
git clone --no-checkout git@github.com:adriangitvitz/doomconfig.git
cd doomconfig
git sparse-checkout init --cone
git sparse-checkout set themes
git checkout main
```

Load the theme ~/.config/doom/packages.el

``` emacs-lisp
(package! doom-kera-theme
  :recipe (:local-repo "themes"
           :files ("*.el")))
```

Changes in ~/.config/doom/config.el

``` emacs-lisp
(setq doom-theme 'doom-kera)
```

Reload your config

``` shell
~/.config/emacs/bin/doom sync
```

* Notes:
doom-kera-theme.el should be inside themes folder under ~/.config/doom

``` text
~/.config/doom/themes/doom-kera-theme.el
```

![Screenshot 2025-04-11 at 1 27 08 p m](https://github.com/user-attachments/assets/0d185e3b-7d58-4b48-a1cd-b6571d9a938c)
