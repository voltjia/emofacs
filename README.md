# Emofacs

Emofacs is my own Emacs configuration.

## Introduction

"Emofacs" == "E" + "mofa" + "cs", which is a combination of "Emacs", the name of the famous editor, and "mofa", pinyin for the Chinese word for "magic". As a big fan of Emacs, I store my configuration in this repository both as a backup in case my local one is lost and for sharing. Anybody interested in my configuration may feel free to try it.

## Dependency

* [Emacs 27.2](https://www.gnu.org/software/emacs/)

This is my Emacs version, but Emofacs should also work in some older versions.

* [Language Servers](https://langserver.org/)

A language server is only necessary when you want to code in a specific programming language. You may choose from the available servers listed in the web page above. For example, if you would like to code in C/C++, then installing LLVM is a good idea, which contains clangd, a language server for C/C++. Most of the time, when there is no server detected for a specific source file, LSP mode prompts you whether to install a recommended one, then simply type `yes` to do so. However, there might be situations when LSP mode does not prompt, then please follow the installation instructions of a language server of your choice from the above web page.

## Installation

For the convenience to configure an empty Emacs as simple as possible, the straight package manager (straight.el) and use-package are used, and the only file you really need is `init.el`.

Simply put `init.el` into `.emacs.d`, the standard location for additional per-user Emacs-specific files, Emacs is going to configure itself automatically during the next startup. Or if you want, you may git clone this whole repository as your `.emacs.d`. In this way, you may update this configuration using git in the future. For more information about `.emacs.d`, please visit https://www.emacswiki.org/emacs/DotEmacsDotD.

The installation time depends on your Internet connection and CPU, because the packages need to be cloned and built. It takes about 3 minutes on my personal computer at home. During installation, Emacs may seem to be stuck, but it is not, so just wait patiently, but if the installation is not complete after 20 minutes, then there might be something wrong. In order to use `all-the-icons` package, Emacs may prompt you to install `all-the-icons` fonts if none detected, type `yes` when prompted, or you may choose to disable features using `all-the-icons`, although in this way Emofacs may look less pretty.

Even if everything goes well, Emacs may still display some warnings and errors after initialization, and the `all-the-icons` theme may not be loaded the first time Emacs starts up. Just ignore them and restart Emacs, and then Emofacs should work properly.

## Packages

Here is a list of packages used in Emofacs:

* [ace-window](https://github.com/abo-abo/ace-window) - Quickly switch windows.
* [all-the-icons](https://github.com/domtronn/all-the-icons.el) - A package used to include fancy icons within emacs.
* [Company](https://company-mode.github.io/) - A text completion framework.
* [Dashboard](https://github.com/emacs-dashboard/emacs-dashboard) - A startup dashboard which provides certain information about your recent Emacs activities.
* [exec-path-from-shell](https://github.com/purcell/exec-path-from-shell) - Get environment variables such as $PATH from the shell for Mac user.
* [Flycheck](https://www.flycheck.org/en/latest/) - Modern on-the-fly syntax checking meant to be a replacement to FlyMake.
* [Iedit](https://github.com/victorhge/iedit) - Modify multiple occurrences in the same way simultaneously.
* [Ivy](https://github.com/abo-abo/swiper) - Flexible and simple tools for minibuffer completion in Emacs.
* [LSP](https://emacs-lsp.github.io/lsp-mode/) - Emacs client for the Language Server Protocol.
* [Magit](https://magit.vc/) - Interacting with git.
* [powerline](https://github.com/milkypostman/powerline) - Emacs version of the Vim powerline.
* [Projectile](https://github.com/bbatsov/projectile) - Project Interaction Library for Emacs.
* [Smartparens](https://github.com/Fuco1/smartparens) - Deals with parens pairs and tries to be smart about it.
* [straight](https://github.com/raxod502/straight.el) - Next-generation, purely functional package manager for the Emacs hacker.
* [Treemacs](https://github.com/Alexander-Miller/treemacs) - A tree layout file explorer for Emacs.
* [use-package](https://github.com/jwiegley/use-package) - A declaration macro to isolate package configuration in a way that is performance-oriented and tidy.
* [which-key](https://github.com/justbur/emacs-which-key) - Display available key bindings in popup. Rewrite of guide-key with added features to improve display.
* [Zenburn](https://github.com/bbatsov/zenburn-emacs) - Vim’s “Zenburn theme” ported to Emacs.

You may also want to find some interesting packages yourself. Then this community driven list of useful Emacs packages, libraries, and others - [Awesome Emacs](https://github.com/emacs-tw/awesome-emacs) - might be helpful.

## Usage

The learning curve of Emacs is steep, but once you get it, you may feel the power that Emacs brings you. With emacs, you may discover that a mouse is not that important. Most actions can be done using your keyboard without your hands moving away.

However, I am not going to teach you how to use Emacs or any of the specific packages mentioned above. Instead, I have listed their official websites or GitHub links so that you may easily find documents on each of them and begin exploring.

## License

MIT License © Jiacheng Huang
