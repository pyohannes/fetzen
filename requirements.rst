FETZEN requirements
===================

Definition of terms
-------------------
+----------------------+----------------------------------------------------+
| FETZEN source        | One file that contains both code and documentation |
|                      | code.                                              |
+----------------------+----------------------------------------------------+
| code source          | The code extracted from the FETZEN source.         |
+----------------------+----------------------------------------------------+
| program              | The compiled code.                                 |
+----------------------+----------------------------------------------------+
| documentation source | The documentation code extracted from the FETZEN   |
|                      | source.                                            |
+----------------------+----------------------------------------------------+
| documentation        | The processed documentation in its final form.     |
+----------------------+----------------------------------------------------+


Summary
-------

The primary purpose of FETZEN is to separately extract code source and
documentation source from one single FETZEN source file where both are 
entangled.

Requirements
------------

1) It must be possible to separatly extract code source and documentation 
   source from one single file.

2) It must be possible to extract code source and documentation source in 
   separate files.

3) Code must be embedded into the documentation source.

4) Code parts can be marked so that they are not embedded into the
   documentation source.

5) It must be possible to add comments that are not extracted from the FETZEN
   source.

6) It must be possible to apply customizable formatting to code source and
   documentation source.

7) It must be possible to preserve line numbers in the code source and 
   documentation source.

Restrictions
------------

1) FETZEN works on single files.

