# CHANGELOG

### 0.7.2 (May 21, 2017)

* Fix a bug where opening a buffer with sqlup-mode as a hook might upcase the first word accidentally

### 0.7.1 (Dec 7, 2016)

* If major mode is sql-mode, do not create indirect buffer, just use it directly (#48)
* This also fixes a problem with font-lock mode breaking when sqlup-mode is disabled (#63)

### 0.7.0 (Sep 4, 2016)

* Add a list of words that can be excluded (sqlup-blacklist)
* Keyboard macros no longer break sqlup-mode

### 0.6.2 (Sep 2, 2016)

* \c is no longer capitalized in SQL-interactive mode for Postgres
* Many thanks to David Shepherd (davidshepherd7 on Github) for all the improvements

### 0.6.1 (Aug 26, 2016)

* Stop capitalizing words containing underscores which partially match a keyword
* Set up Travis-CI
*

### 0.6.0 (Jun 24, 2016)

* Use `clone-indirect-buffer` instead of a temp buffer
* Fix defect where the symbol before point was targeted by logic when enabling sqlup-mode
* Improve docstrings

### 0.5.9 (Jun 8, 2016)

* Support for upcasing within strings inside `EXECUTE format(`
* Tests!

### 0.5.8 (May 30, 2016)

* Variable renaming, slight increase in readability

### 0.5.7 (May 30, 2016)

* Add support for upcasing keywords within eval strings (psql only)

### 0.5.6 (May 25, 2016)

* Increase support for older emacsen
* Fix the code
* Continue thinking about how nice it'd be to have tests for this

### 0.5.5 (May 24, 2016)

* Fix bug: I can't use string-replace apparently

### 0.5.4 (May 23, 2016)

* Support for redis-mode (provided by `redis` package)
* Multi-word predicate functions end in `-p`
* Untabify

### 0.5.3 (Oct 4, 2015)

* Hi, people who read Changelogs. I dropped the ball a bit.
* sqlup now will use keywords based on the correct SQL product

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
