* Parser: Fix parsing of trailing whitespaces in list
* Support different output formats (e.g. not just markdown)
* Scribble-like syntax for comments
    * Have a function call syntax e.g. `@function{parameter}@`
    * Map that to formatters to allow stuff like `@pre{code}@`
    * Should also allow for customizations through the SchemeDoc library
* Somehow allow for a semantic representation of scheme objects
  in the Formatable type class, e.g. don't already return specific
  heading levels etc
* Don't return a tuple in defFormatter
* Parser: Better error messages
* Tests:
    * Add integration tests (`golden/`)
    * Perform some compliance tests of the R7RS parser
* Make it easier to use SchemeDoc as a library
    * E.g. for supplying custom expression formatters
