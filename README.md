## Doom emacs Keratoconus

![Screenshot 2025-04-15 at 12 12 02 a m](https://github.com/user-attachments/assets/85d015e4-71b8-4aea-b378-e23a18286814)

WIP: Markdown Live-Preview using [glow](https://github.com/charmbracelet/glow)

![Screenshot 2025-04-15 at 1 40 55 p m](https://github.com/user-attachments/assets/e7a95565-2f01-401b-bc69-a6eb6132894f)


WIP: LeetCode Problem Viewwer


![Screenshot 2025-04-17 at 4 27 28 a m](https://github.com/user-attachments/assets/a439c7ab-709f-424e-bfea-66dded28598b)


### How to install Theme

Clone kera theme by using sparse checkout

``` shell
git clone --no-checkout git@github.com:adriangitvitz/doomconfig.git
cd doomconfig
git sparse-checkout init --no-cone
echo "themes/" > .git/info/sparse-checkout
git checkout
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


