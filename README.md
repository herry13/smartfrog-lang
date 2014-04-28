[SmartFrog](http://smartfrog.org) Language Semantics
============================
This is the implementation of the formal semantic of [SmartFrog](http://smartfrog.org) (SF) language in Scala. The supported features are:

- Basic values (Boolean, Number, String, Null, Vector)
- Data and Link Reference (_forward link reference_ is not supported)
- Keywords `THIS`, `PARENT`, and `ROOT` in Reference
- Prototype
- Placement (_forward placement_ is not supported)
- Include file `#include "any.sf";`

Not yet supported:

- `TBD`
- Optional value of link reference
- Keywords `ATTRIB`, `HERE` in reference
- Lazy reference
- Function
- Schema
- Predicate

Note: forward link-reference is supported in branch `forward-link-reference`.

Requirements
------------
To run:
- Java Virtual Machine >= 1.6

To build:
- Scala >= 2.10.3
- SBT >= 0.13 (optional) -- only require to create an independent JAR file


Usage
-----

	$ ./sfParser [option] <sf-file>

To generate standard output for input file: test.sf

	$ ./sfParser test.sf

To generate JSON output

	$ ./sfParser -json test.sf

To generate YAML output

	$ ./sfParser -yaml test.sf

Note that every value is converted as it is to JSON and YAML.
But the data-reference will be converted to string preceeded with
characters: `$.`. For example, data-reference `x:y:z` in JSON and
YAML is `$.x:y:z`.


Independent JAR file
--------------------
An independent JAR file is available at [here](https://github.com/herry13/smartfrog-lang/blob/master/sbt/target/scala-2.10/sfParser-assembly-0.3.jar).
The JAR file can be executed with command:

	$ java -jar sbt/target/scala-2.10/sfParser-assembly-0.3.jar <sf-file>

To build an independent JAR file:

	$ cd sbt
	$ sbt assembly


Output
------
If you have the following input:

	A extends {
		foo 1;
	}
	
	sfConfig extends {
		test extends {
			bar 1;
			a1 extends A
		}
		bar 2;
		a2 test:a1;
	}

then the output will be:

	(test,{(bar,1),(a1,{(foo,1)})}),(bar,2),(a2,{(foo,1)})

`(x,y)` means that variable `x` has a basic value `y`, and
`(x,{...})` means that variable `x` is a component (object).


License
-------
[BSD License](https://raw.githubusercontent.com/herry13/smartfrog-lang/master/LICENSE)
