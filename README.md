Build status [<img src="https://secure.travis-ci.org/Trevoke/sqlup-mode.el.png" />](http://travis-ci.org/Trevoke/sqlup-mode.el)

# sqlup-mode

`sqlup-mode` is a minor mode for emacs. Its sole purpose is to make
your life easier when writing SQL.

### Purpose

SQL, by convention, uses upper-case keywords, although lower-case
works just as well. As humans, the separation between upper-case and
lower-case helps scan and parse the code much more quickly.

This mode has been extended to upcase keywords when using `redis-mode`
as well.

### Pronunciation

I gave a talk [(slides)](https://blog.trevoke.net/sqlup-talk/) at the
emacs NYC meetup, and it was decided that `sqlup` is pronounced `skloop`.

## Installation

### Manually

Content TK. Pull request welcome. Same as all other emacs packages,
really.

### MELPA

[![MELPA](http://melpa.org/packages/sqlup-mode-badge.svg)](http://melpa.org/#/sqlup-mode) [![MELPA Stable](http://stable.melpa.org/packages/sqlup-mode-badge.svg)](http://stable.melpa.org/#/sqlup-mode)


### Marmalade

sqlup-mode is NOT YET on Marmalade.

## Usage

### Basic setup

Here follows an example setup to activate `sqlup-mode` automatically when entering sql-mode or sql-interactive-mode:

``` lisp
;; Capitalize keywords in SQL mode
(add-hook 'sql-mode-hook 'sqlup-mode)
;; Capitalize keywords in an interactive session (e.g. psql)
(add-hook 'sql-interactive-mode-hook 'sqlup-mode)
;; Set a global keyword to use sqlup on a region
(global-set-key (kbd "C-c u") 'sqlup-capitalize-keywords-in-region)
```

### Blacklisting words

Sqlup can be configured to ignore certain keywords by adding them to the list
`sqlup-blacklist`. For example if you use `name` as a column name it would be
annoying to have it upcased so you can prevent this by adding

```lisp
(add-to-list 'sqlup-blacklist "name")
```

to your config (or do the equivalent through the `M-x customize` interface).

### Normal typing (e.g. SQL REPL)

Activate the minor mode with `M-x sqlup-mode` and you can just start
typing. The minor mode will be triggered by the following keys:
* `SPC`
* `(`
* `,`
* `;`
* `RET`
* `'`

### Work with a region

Select a region and just call `M-x sqlup-capitalize-keywords-in-region`.
Magic.

### Work with a whole buffer

Just call `M-x sqlup-capitalize-keywords-in-buffer`.

## Implementation choices

I made the choice of only triggering the word-scanning when a particular keypress happens specifically because I don't want to see the word "ORde" typed when I'm typing "ORDER", and I didn't know a *simple* way to do it. I believe that in practice, this is good enough.

