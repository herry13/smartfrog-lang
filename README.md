smartfrog-lang
==============
This is the implementation of the formal semantic of SmartFrog (SF) language in Scala. The supported features are:

- Primitive Value
- Data and Link Reference + late-binding of link reference
- Vector
- Prototype
- Placement (no late-binding)

Note that the _lazy reference_ is treated same as data reference.

Not yet supported:

- TBD
- Optional value of link reference
- Keywords in reference: ROOT, PARENT, ATTRIB, HERE, THIS
- Function
- Schema
- Predicate


Requirements
------------
To run:
- Java Virtual Machine >= 1.6

To build:
- Scala >= 2.10.3
- SBT >= 0.13 (optional) -- only require to create an independent JAR file


Usage
-----

	$ ./sfParser <sf-file>


Independent JAR file
--------------------
An independent JAR file is available at:

https://github.com/herry13/smartfrog-lang/blob/master/sbt/target/scala-2.10/sfParser-assembly-0.1.jar

The JAR file can be executed with command:

	$ java -jar sbt/target/scala-2.10/sfParser-assembly-0.1.jar <sf-file>


Output
------
If you have the following input:

	A extends {
		foo bar;
	}
	
	sfConfig extends {
		test extends {
			bar 1;
			a1 extends A;
		}
		bar 2;
		a2 test:a1;
	}

then the output will be:

	(test,{(bar,1),(a1,{(foo,1)})}),(bar,2),(a2,{(foo,1)})

`(x,y)` means that variable `x` has primitive value `y`, and
`(x,{...})` means that variable `x` is a component.


License
-------
BSD License
