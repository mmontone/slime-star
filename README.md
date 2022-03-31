# SLIME ⭐

Overview
--------

This is a [SLIME](https://slime.common-lisp.dev/) configuration that provides the following contribs:

- [Lisp System Browser](https://github.com/mmontone/lisp-system-browser)
- [SLIME Doc Contribs](https://github.com/mmontone/slime-doc-contribs)
- [Quicklisp Systems](https://github.com/mmontone/quicklisp-systems)
- [Quicksearch utility](https://github.com/tkych/quicksearch)
- [SLIME Breakpoints](https://github.com/mmontone/slime-breakpoints)
- [Slite](https://github.com/tdrhq/slite/)

It also adds some custom utilities and menus.

Install
-------

⚠️ **This is ALPHA software**

Clone this repository using --recursive option (this repo has git submodules):

```
git clone --recursive https://github.com/mmontone/slime-star.git
```

Install some dependencies from Quicklisp:

```lisp
(ql:quickload '(:asdf :alexandria :anaphora :drakma :dexador :fiveam :closer-mop :iterate :do-urlencode :yason :html-entities :slite))
```

In your `.emacs`:

```elisp
;; Setup load-path, autoloads and your lisp system
(add-to-list 'load-path "~/dir/to/cloned/slime-star")

;; Add slime-star to slime-contribs:
(setq slime-contribs '(slime-fancy slime-star))
```

## Screenshots

![toolbars](screenshots/toolbars.png "Stepping using toolbar")

License
-------

SLIME :star: is free software. All files, unless explicitly stated otherwise, are public domain.
