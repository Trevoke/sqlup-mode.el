# slqup-mode

`sqlup-mode` is a minor mode for emacs. Its sole purpose is to make
your life easier when writing SQL.

SQL, by convention, uses upper-case keywords, although lower-case
works just as well. As humans, the separation between upper-case and
lower-case helps scan and parse the code much more quickly.

## Installation

### Manually

Content TK. Pull request welcome. Same as all other emacs packages,
really.

### MELPA

sqlup-mode is on MELPA.

### Marmalade

sqlup-mode is NOT YET on Marmalade.

## Basic Usage

### Normal typing (e.g. SQL REPL)

Activate the minor mode with `M-x sqlup-mode` and you can just start
typing. The minor mode will be triggered by the following keys: `SPC`,
`(`, `,`, and `;` .

### Work with a region

Select a region and just call `M-x sqlup-capitalize-keywords-in-region`.
Magic.
