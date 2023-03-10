* Support `include-ci` in `Library.hs`
* Add flag to include un-exported identifiers in documentation
* Consider using `lucid2` instead of `blaze-html`
    * Ideally in combination with `mmark`
    * See: https://github.com/mmark-md/mmark
* Scribble-like syntax for comments
    * Have a function call syntax e.g. `@function{parameter}@`
    * Map that to formatters to allow stuff like `@pre{code}@`
    * Should also allow for customizations through the SchemeDoc library
* Somehow allow for a semantic representation of scheme objects
  in the Formatable type class, e.g. don't already return specific
  heading levels etc
* Parser: Better error messages
* Tests: Perform some compliance tests of the R7RS parser
* Make it easier to use SchemeDoc as a library
    * E.g. for supplying custom expression formatters
