# Intro

`tshell` is an experimental buffer-oriented Emacs shell.

It is built around several basic ideas:

- `RET` runs current line, so you can easily iterate on your one-liners
- line prompt determines interpreter: `$` for shell, `>` for elisp
- output by default goes to a single `*tshell-out*` buffer
- to get a named shell with history, write tshell buffer to a file, like `~/.tshell_history`
- contents of the `*tshell-out*` buffer can be easily used as input for commands
- create shortcuts via [transient](https://github.com/magit/transient)

## Demo
See https://imgur.com/a/dsdKG2D

## Installation
```sh
git clone https://github.com/magit/transient ~/.emacs.d/
```

```elisp
(use-package tshell
  :after transient
  :load-path "~/.emacs.d/tshell")
```

## Usage

Run `M-x tshell`.
You can change interpreter by just editing prompt character:
```
$ # For example type `C-a C-k >` to get
> ;; elisp interpreter
```
If you want to create a new line without evaluating anything, type `C-j` or `M-j`.

### Current working directory

Current working directory is determined as in any other buffer by the
value of local `default-directory` variable:
```
> default-directory
```
It is shown in the header line.
You can change it in multiple ways:
```
$ cd /tmp
$ pwd
```
Typing `C-c C-d` is equivalent to `M-x cd`. That means it uses your favorite completion mechanism!

#### ls
You probably want to see the contents of the current directory quite often.

The easiest way to do that is to type `C-c C-l`.

### Input/Output redirection
`tshell` does almost nothing to your command, so you can do pipes and redirections as usual:
```
$ cd ~/.emacs.d/tshell
$ # Here I show multiple lines, but actually I just edit the same line
$ ls
$ ls | grep el$
$ ls | grep el$ > /tmp/files
$ # And now open it
$ e /tmp/files
```

But most of the time you just want to pass contents of your `*tshell-out*` to the next command.
```
$ ls *md
$ > sed 's/md/org/' # Here I'm using `>` to send contents of the `*tshell-out*` to stdin of `sed`
```
If you realized that you need to rerun the command,
you can either type `C-p RET C-n` to recreate desired input
or you can undo changes in the `*tshell-out*` buffer.

#### Undo
To undo changes in `*tshell-out*` buffer you can run
```
:undo
```
or just switch to the `*tshell-out*` buffer and use your regular undo keybinding, like `C-/`.

#### History variables
Every time you evaluate a lisp command, result of the evaluation is stored in variable `*`
```
> (+ 13 69) # 82
> (+ 42 *) # 124
```
`*` is a regular elisp variable, so you can use it in any other context, like `*scratch*` buffer or `M-x eval`.

### Misc
You can type `C-c C-y` to insert contents of the `*tshell-out*` buffer at point:
```
$ locate tshell | grep 'el$'
$ e # Type `C-c C-y`
$ e /home/tatrix/.emacs.d/tshell/tshell.el # `e` is a special command which opens file in the emacs buffer
```
