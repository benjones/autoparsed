# Autoparsed

Autoparsed is a library to automatically generate parsers.

Users define the grammar by writing classes for the various AST nodes and annotating their constructors.  Users define a set of enums (constants) and structs describing the tokens.  Then, for each AST node, provide an annotated constructor which describes the tokens needed to make one of those nodes.  The library automatically generates a recursive descent parser that returns an AST.  It supports (understands *, +, and | now, more coming) basic PEG grammar definitions, but allows you define the grammar as a part of your data types.

See the [example](example/) folder for an example that makes use of it.  

You can run the example code with `dub run autoparsed:autoparsedExample` which will lex and parse S-expressions.  Check out [the sexp grammar](example/sexpGrammar.d) to see how to use the library.
