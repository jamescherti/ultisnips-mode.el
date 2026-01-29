# ultisnips-mode.el
![Build Status](https://github.com/jamescherti/ultisnips-mode.el/actions/workflows/ci.yml/badge.svg)
[![MELPA](https://melpa.org/packages/ultisnips-mode-badge.svg)](https://melpa.org/#/ultisnips-mode)
[![MELPA Stable](https://stable.melpa.org/packages/ultisnips-mode-badge.svg)](https://stable.melpa.org/#/ultisnips-mode)
![License](https://img.shields.io/github/license/jamescherti/ultisnips-mode.el)
![](https://jamescherti.com/misc/made-for-gnu-emacs.svg)

The **ultisnips-mode** is an Emacs major mode for editing Ultisnips snippet files (*.snippets files). This mode provides syntax highlighting to facilitate editing Ultisnips snippets.

*(Vim's UltiSnips is a snippet solution for Vim, and its snippets can be used in Emacs by converting them to the Yasnippet format using [Ultyas](https://github.com/jamescherti/ultyas).)*

## Features

* Syntax highlighting for UltiSnips: `snippet`, `endsnippet`, `global`, `endglobal`, `priority`, comments, and placeholders in the form of `${1:value}` or `$1`...
* Integration with `outline-minor-mode` or `hs-minor-mode` to enable folding of `snippet` -> `endsnippet` and `global` -> `endglobal` blocks.
* Support for commenting and uncommenting UltiSnips snippets using standard Emacs commands, such as `comment-or-uncomment-region`.
* Add `*.snippets` to `auto-mode-alist` to automatically enable `ultisnips-mode`.

## Installation

To install *ultisnips-mode* from MELPA:

1. If you haven't already done so, [add MELPA repository to your Emacs configuration](https://melpa.org/#/getting-started).

2. Add the following code to your Emacs init file to install *ultisnips-mode* from MELPA:
```emacs-lisp
(use-package ultisnips-mode
  :ensure t)
```

## Frequently Asked Questions

### Folding snippet blocks with hs-minor-mode

Activating `hs-minor-mode` provides the ability to collapse and expand `snippet` -> `endsnippet` blocks, making navigation in large snippet files much easier.

To enable `hs-minor-mode` automatically for Ultisnips files, add the following to the Emacs configuration:

```emacs-lisp
(add-hook 'ultisnips-mode-hook #'hs-minor-mode)
```

NOTE: As an alternative to `hs-minor-mode`, `ultisnips-mode` also supports `outline-minor-mode`; however, **`hs-minor-mode` is recommended because it can reliably fold entire blocks from `snippet` to `endsnippet`**. In contrast, `outline-minor-mode` uses only the `snippet` line as a header and may hide everything between the first `snippet` and the next one, including comments.

For a better and more intuitive way to fold and unfold snippets, it is recommended to use the [kirigami.el](https://github.com/aki2o/kirigami) emacs package.

### Hiding all snippets with hs-minor-mode when opening *.snippets files

The following configuration automatically folds all code blocks when `ultisnips-mode` is enabled, collapsing all snippets:

```elisp
(defun my-ultisnips-mode-fold-all ()
  "Fold all snippet blocks using `hs-minor-mode'."
  (hs-minor-mode 1)
  (hs-hide-all))

(add-hook 'ultisnips-mode-hook #'my-ultisnips-mode-fold-all)
```

This ensures a cleaner view of snippets, collapsing all regions by default and allowing selective expansion as needed.

### Is there a package that allows using UltiSnips directly in Emacs, or is it necessary to first convert them to the Yasnippet format?

Yes, conversion to Yasnippet is required.

A command-line tool called [Ultyas](https://github.com/jamescherti/ultyas) facilitates the conversion of snippets from UltiSnips to Yasnippet format.

### How does the author use ultisnips-mode and Ultyas?

The author maintains all snippets in the original UltiSnips format for Vim and uses [Ultyas](https://github.com/jamescherti/ultyas) to convert them automatically to Yasnippet format for Emacs.

He uses **ultisnips-mode** in Emacs to edit snippets, providing syntax highlighting and code folding through **hs-minor-mode**.

This workflow allows editing and storing a single set of snippets while making them available in Vim and Emacs.

The author wrote a shell script that automatically scans the UltiSnips directory and generates the corresponding Yasnippet files whenever snippets are updated, ensuring synchronization between Vim and Emacs.

## Author and License

The *ultisnips-mode* Emacs package has been written by [James Cherti](https://www.jamescherti.com/) and is distributed under terms of the GNU General Public License version 3, or, at your choice, any later version.

Copyright (C) 2025-2026 James Cherti

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program.

## Links

- [ultisnips-mode.el @GitHub](https://github.com/jamescherti/ultisnips-mode.el)
- [Ultyas](https://github.com/jamescherti/ultyas/): A command-line tool designed to simplify the process of converting code snippets from UltiSnips to YASnippet format.

Other Emacs packages by the same author:
- [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d): This repository hosts a minimal Emacs configuration designed to serve as a foundation for your vanilla Emacs setup and provide a solid base for an enhanced Emacs experience.
- [compile-angel.el](https://github.com/jamescherti/compile-angel.el): **Speed up Emacs!** This package guarantees that all .el files are both byte-compiled and native-compiled, which significantly speeds up Emacs.
- [easysession.el](https://github.com/jamescherti/easysession.el): Easysession is lightweight Emacs session manager that can persist and restore file editing buffers, indirect buffers/clones, Dired buffers, the tab-bar, and the Emacs frames (with or without the Emacs frames size, width, and height).
- [vim-tab-bar.el](https://github.com/jamescherti/vim-tab-bar.el): Make the Emacs tab-bar Look Like Vim’s Tab Bar.
- [elispcomp](https://github.com/jamescherti/elispcomp): A command line tool that allows compiling Elisp code directly from the terminal or from a shell script. It facilitates the generation of optimized .elc (byte-compiled) and .eln (native-compiled) files.
- [tomorrow-night-deepblue-theme.el](https://github.com/jamescherti/tomorrow-night-deepblue-theme.el): The Tomorrow Night Deepblue Emacs theme is a beautiful deep blue variant of the Tomorrow Night theme, which is renowned for its elegant color palette that is pleasing to the eyes. It features a deep blue background color that creates a calming atmosphere. The theme is also a great choice for those who miss the blue themes that were trendy a few years ago.
- [dir-config.el](https://github.com/jamescherti/dir-config.el): Automatically find and evaluate .dir-config.el Elisp files to configure directory-specific settings.
- [flymake-bashate.el](https://github.com/jamescherti/flymake-bashate.el): A package that provides a Flymake backend for the bashate Bash script style checker.
- [flymake-ansible-lint.el](https://github.com/jamescherti/flymake-ansible-lint.el): An Emacs package that offers a Flymake backend for ansible-lint.
- [inhibit-mouse.el](https://github.com/jamescherti/inhibit-mouse.el): A package that disables mouse input in Emacs, offering a simpler and faster alternative to the disable-mouse package.
- [quick-sdcv.el](https://github.com/jamescherti/quick-sdcv.el): This package enables Emacs to function as an offline dictionary by using the sdcv command-line tool directly within Emacs.
- [enhanced-evil-paredit.el](https://github.com/jamescherti/enhanced-evil-paredit.el): An Emacs package that prevents parenthesis imbalance when using *evil-mode* with *paredit*. It intercepts *evil-mode* commands such as delete, change, and paste, blocking their execution if they would break the parenthetical structure.
- [stripspace.el](https://github.com/jamescherti/stripspace.el): Ensure Emacs Automatically removes trailing whitespace before saving a buffer, with an option to preserve the cursor column.
- [persist-text-scale.el](https://github.com/jamescherti/persist-text-scale.el): Ensure that all adjustments made with text-scale-increase and text-scale-decrease are persisted and restored across sessions.
- [pathaction.el](https://github.com/jamescherti/pathaction.el): Execute the pathaction command-line tool from Emacs. The pathaction command-line tool enables the execution of specific commands on targeted files or directories. Its key advantage lies in its flexibility, allowing users to handle various types of files simply by passing the file or directory as an argument to the pathaction tool. The tool uses a .pathaction.yaml rule-set file to determine which command to execute. Additionally, Jinja2 templating can be employed in the rule-set file to further customize the commands.
- [kirigami.el](https://github.com/jamescherti/kirigami.el): The *kirigami* Emacs package offers a unified interface for opening and closing folds across a diverse set of major and minor modes in Emacs, including `outline-mode`, `outline-minor-mode`, `outline-indent-mode`, `org-mode`, `markdown-mode`, `vdiff-mode`, `vdiff-3way-mode`, `hs-minor-mode`, `hide-ifdef-mode`, `origami-mode`, `yafolding-mode`, `folding-mode`, and `treesit-fold-mode`. With Kirigami, folding key bindings only need to be configured **once**. After that, the same keys work consistently across all supported major and minor modes, providing a unified and predictable folding experience.
