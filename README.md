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
- Scala >= 2.10
- Java Virtual Machine >= 1.6


Usage
-----

	$ ./sfParser <sf-file>


License
-------
BSD License
