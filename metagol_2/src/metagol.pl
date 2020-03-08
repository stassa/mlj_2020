:-module(metagol_shim, []).

:-reexport('../metagol.pl').

/** <module> Metagol shim layer.

This module exists to maintain compatibility of the directory structure
in Metagol with the directory structure in Louise.

In Louise, learning predicates, in particular learn/5, are defined in
the module called "louise" located inside the directory louse/src.
Metagol has a flat directory structure and the main module, metagol.pl
is located in the root directory of the Metagol project.

Experiment code in experiments/learning_rate/scripts/learning_rate.pl is
possible to simplify considerably if it can find Metagol's learning
predicates in metagol/src, just as it can with louise/src (the
alternative is to maintain a separate directory path depending on the
learner, which is rather a pain). This module allows the learning
predicates defined by the two learners to be called in a consistent
manner by re-exporting the module metagol.pl found in Metagol's root
directory.

Note that the name of this _file_ is metagol.pl (metagol/src/metagol.pl)
whereas the name of this _module_ is metagol_shim. To clarify, Swi's
module system means that the predicates defined in the module named
"metagol" (in the file metagol/metagol.pl) and re-exported by the module
named "metagol_shim" are still found in the dynamic database as
metagol:<predicate> terms. Bit confusing but gets the job done.
*/
