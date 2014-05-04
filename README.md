EmacsPlusPlus is an extensible Clojure-centric Emacs Setup
----------------------------------------------

* Add your user-specific code to `~/.emacs.d/userspecific`

### General Purpose

Keybinding         | Description
-------------------|------------------------------------------------------------
<kbd>qq</kbd> | Rotate buffers
<kbd>ii</kbd> | Cursor jump up 15 lines
<kbd>kk</kbd> | Cursor jump down 15 lines
<kbd>"jj"</kbd> | 'ace-jump-word-mode
<kbd>"jc"</kbd> | 'ace-jump-char-mode
<kbd>",."</kbd> | 'er/expand-region
<kbd>"ww"</kbd> | 'wgrep-change-to-wgrep-mode
<kbd>"xx"</kbd> | 'gui-diff-last-failure
<kbd>C-x f</kbd> | Open file in sudo
<kbd>S-C-<left></kbd> | Shrink window left
<kbd>S-C-<right></kbd> | Shrink window right
<kbd>S-C-<up></kbd> | Shrink window up
<kbd>S-C-<down></kbd> | Shrink window down
<kbd>C-c n</kbd> | Cleanup file
<kbd>C-x g</kbd> | Search the web
<kbd>C-.</kbd> | 'mc/mark-next-like-this)
<kbd>C-,</kbd> | 'mc/mark-previous-like-this)
<kbd>C-!</kbd> | 'mc/mark-all-like-this)
<kbd>C-S-c C-S-c</kbd> | 'mc/edit-lines)
<kbd>`<S-up>`</kbd>    | 'buf-move-up
<kbd>`<S-down>`</kbd>  | 'buf-move-down
<kbd>`<S-left>`</kbd>  | 'buf-move-left
<kbd>`<S-right>`</kbd> | 'buf-move-right

Function        | Description
----------------|---------------------------------------------------------------
`save-macro` | Saves last macro and stores it in `~/.emacs.d/core.el`
`emacs++-insert-date` | Prints date at point, like 'Sun, May  4, 2014'

### Clojure-Specific

* clj-refactor-mode prefix: <kbd>C-c C-x</kbd>

Keybinding       | Description
----------------|---------------------------------------------------------------
<kbd>C-></kbd> | 'cljr-thread
<kbd>C-<</kbd> | 'cljr-unwind
<kbd>M-C-></kbd> | 'cljr-thread-first-all
<kbd>M-C-?</kbd> | 'cljr-thread-last-all
<kbd>c[</knd> | 'cljr-cycle-coll)
<kbd>i[</knd> | 'cljr-cycle-if)
<kbd>p[</knd> | 'cljr-cycle-privacy)
<kbd>s[</knd> | 'cljr-cycle-stringlike)
<kbd>xx</kbd> | Extracts last test failure to into a gui-diff fn call

### Git-Specific

Keybinding       | Description
----------------|---------------------------------------------------------------
`g-blame` | Git blame for current line
`g-churn` | Git info for which files churn most in the repo
`g-who`   | Git info for who edited current file most in past 6 months
