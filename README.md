SFP Language Compiler
=====================

This is the implementation of the formal semantic of SFP language. Supporting features are:
- Static type system
- Static schema
- Basic values: boolean, number, string, null, and vector
- Data and link references (including _forward_ references)
- Object prototypes
- Actions
- Global constraints
- File inclusion
- Keywords: `root`, `parent`, and `this`
- Finite Domain Representation ([FDR](http://www.fast-downward.org/TranslatorOutputFormat)) generator for planning

Currently, only the [OCaml](ocaml/) implementation is working. The [Scala](scala/) implementation is still under development.

To build
--------
Requirement:
- OCaml >= 3.12.1

Compile:

	make dist

The above command will generate file `csfp`. In default, the codes are compiled into OCaml bytecodes which requires OCaml runtime for running. However, we can compile the codes into native bytecodes by setting variable `NATIVE=1` in `Makefile`. This change will generate the same file i.e. `csfp`, but this is a native executable file which can be invoked without any OCaml runtime.

Usage
-----
The simplest command to use `csfp` is

	$ ./csfp spec.sfp

This will parse the specification and generate the final result in JSON. In addition, there are several options that can be used:
- `./csfp -ast spec.sfp` : print an abstract syntax tree of the specification, which is very useful to quickly find any syntax error.
- `./csfp -type spec.sfp` : evaluate the types of every element and check any type-error on the specification.
- `./csfp -json spec.sfp` : evaluate every element and generate the final result in JSON.
- `./csfp -yaml spec.sfp` : evaluate every element and generate the final result in YAML.
- `./csfp -fs spec.sfp` : print variables and their values in a flat-structure.
- `./csfp -fdr init.sfp goal.sfp` : given an initial state in `init.sfp` and a goal state in `goal.sfp`, generate and print a Finite Domain Representation of this configuration task -- the output can be given to the FastDownward search engine to find a solution plan.
- `./csfp -fd init.sfp goal.sfp` :  given an initial state in `init.sfp` and a goal state in `goal.sfp`, generate and print the solution plan of this configuration task -- note that environment variable `FD_PREPROCESSOR` and `FD_SEARCH` must be set with the path of the FastDownward preprocessor and search engine respectively.


Example
-------

The following files specify the model, the initial, and the goal state of a configuration task.

**model.sfp**:

	// file : model.sfp
	schema Machine {
	  dns = "ns.foo";
	}
	schema Client extends Machine {
	  refer: *Service = null;
	  def redirect(s: Service) {
	    condition { }
	    effect {
	      this.refer = s;
	    }
	  }
	}
	schema Service {
	  running = true;
	  port = 80;
	  def start {
	    condition {
	      this.running = false;
	    }
	    effect {
	      this.running = true;
	    }
	  }
	  def stop {
	    condition {
	      this.running = true;
	    }
	    effect {
	      this.running = false;
	    }
	  }
	}


**init.sfp**:

	include "system1-model.sfp";
	main {
	  s1 isa Machine {
	    web isa Service { }
	  }
	  s2 extends s1, {
	    web.running = false;
	  }
	  pc1 isa Client {
	    refer = s1.web;
	  }
	  pc2 pc1;
	}


**goal.sfp**:

	include "system1-model.sfp";
	main {
	  s1 isa Machine {
	    web isa Service {
	      running = false;
	    }
	  }
	  s2 extends s1, {
	    web.running = true;
	  }
	  pc1 isa Client {
	    refer = s2.web;
	  }
	  pc2 pc1;
	  global {
	    pc1.refer.running = true;
	    pc2.refer.running = true;
	  }
	}


To generate the plan for the above configuration task, invoke the following command

	./csfp -fd init.sfp goal.sfp

Note that environment variable `FD_PREPROCESS` and `FD_SEARCH` must be set before invoking the command.


License
-------
[BSD License](https://raw.githubusercontent.com/herry13/smartfrog-lang/sfp/LICENSE)
