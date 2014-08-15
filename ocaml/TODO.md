TODO
====

refactor:
- save identifiers (strings) to string->integer map to save memory and
  optimise the comparison operations over references
- extract Constraint & Action from sastranslator.ml to constraint.ml and action.ml
- rename sastranslator to fdr

sastranslator.ml:
- implement compilation for simple implications (simple conjunctions) [?]
- implement compilation for simple implications (equalities) [OK]

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
