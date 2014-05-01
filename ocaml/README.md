# SmartFrog Language Compiler in OCaml

This compiler is the implementation of the formal semantic of core subset of [SmartFrog](http://smartfrog.org) (SF) language in OCaml. The supported features are:

- Basic values (Boolean, Number, String, Null, Vector)
- Component (object)
- Data and Link Reference (_forward link reference_ is not supported)
- Keywords `THIS`, `PARENT`, and `ROOT` in Reference
- Prototype
- Placement (_forward placement_ is not supported)

Not yet supported:

- Include file `#include "any.sf";`
- `TBD`
- Optional value of link reference
- Keywords `ATTRIB`, `HERE` in reference
- Lazy reference
- Function
- Schema
- Predicate


## To build

Requirement:

- OCaml (tested on version >= 4.0.1)

Compile:

	make dist


## Usage

	$ ./csf <sf-file>


## License

[BSD License](https://raw.githubusercontent.com/herry13/smartfrog-lang/master/LICENSE)
