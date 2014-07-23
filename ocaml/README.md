# SmartFrog Language Compiler in OCaml

This compiler is the implementation of the formal semantic of core subset of [SmartFrog](http://smartfrog.org) (SF) language in OCaml. The supported features are:

- Basic values (Boolean, Number, String, Null, Vector)
- Component (object)
- Data and Link Reference (_forward link reference_ is supported)
- Prototype
- Placement (_forward placement_ is not supported)
- Include file `#include "any.sf"`

Not yet supported:

- `TBD`
- Optional value of link reference
- Keywords `THIS`, `PARENT`, `ROOT`, `ATTRIB`, `HERE` in reference
- Lazy reference
- Function
- Schema
- Predicate


## To build

Requirement:

- OCaml >= 4.0.1

Compile:

	make dist

This command will create the compiler executable file `csf`.


## Binaries

- [MacOS X](https://raw.githubusercontent.com/herry13/smartfrog-lang/master/ocaml/dist/darwin/csf)
- [Linux](https://raw.githubusercontent.com/herry13/smartfrog-lang/master/ocaml/dist/linux/csf)


## Usage

- JSON output

		$ ./csf <sf-file>

- YAML output

		$ ./csf -yaml <sf-file>

- XML output

		$ ./csf -xml <sf-file>

- Plain SF output

		$ ./csf -sf <sf-file>


## License

[BSD License](https://raw.githubusercontent.com/herry13/smartfrog-lang/master/LICENSE)
