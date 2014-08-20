# CHANGELOG

### 0.4.5 (Aug 20, 2014)

* Do not upcase keywords surrounded by backticks or double-quotes

### 0.4.4 (Aug 13, 2014)

* Fix compatibility with emacs 24.3 and older thing-at-point functionality

### 0.4.3 (Aug 13, 2014)

* Refactors
* Increase consistency of language, internally. Use the `symbol` instead of `word`

### 0.4.2 (Aug 10, 2014)

* Add documentation

### 0.4.1 (Aug 10, 2014)

* Fix region-upcasing logic

### 0.4.0 (Aug 7, 2014)

* Use sql-mode font-lock keywords if available, otherwise use default ANSI keywords

### 0.3.1 (Aug 5, 2014)

* Accidentally removed triggering on `,`
* Update documentation

### 0.3.0 (Aug 4, 2014)

* Use sql-mode's font-lock keywords

### 0.2.0 (Aug 2, 2014)

* Trigger when pressing RETURN
* Trigger when pressing `,`
* Don't trigger when inside a single-line comment prefixed by `--`
* Switch to post-command-hook

### 0.1.4 (Jul 1, 2014)

* Don't crash when checking if whitespace is a keyword

### 0.1.3 (Jun 30, 2014)

* Upcase all the words! Not just the ones who started out all lowercase

### 0.1.2 (Jun 30, 2014)

* Wrap sqlup-capitalize-keywords-in-region with (save-excursion &rest BODY) for the user's sanity

### 0.1.1 (Jun 30, 2014)

* Add autoload cookies

### 0.1.0 (Jun 29, 2014)

* Add sqlup-capitalize-keywords-in-region
* Better list of keywords
* Simpler logic

### 0.0.1 (Jun 25, 2014)

* Initial release
* Basic typing functionality available
