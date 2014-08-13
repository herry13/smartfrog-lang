TODO
====

all files:
- save identifiers (strings) to string->integer map to save memory and
  optimise the comparison operations over references

sastranslator.ml:
- implement compilation for simple implications

preprocessor.ml:
- create the file
- implement function to remove dummy (global) operators
- implement function to convert a total-order to a partial-order plan
- implement function that generates JSON/YAML of a total/partial order plan

csf.ml:
- need to parse all arguments first, and then invoke appropriate functions

Failed tests:
- herry6.sf -- infinite loop
- t1-qc1.sf [Failed]
- t1-qc4.sf [Failed]
- t1-qc5.sf [Failed]
