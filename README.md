TypeNormalizer
==============

TypeRepresentation transformations used in www.quicktypesearch.com.


Given a TypeRepresentation it yields a equivalent TypeRepresentation such any other equivalent TypeRepresentation would yield the same,
therefore, yielding a normalized TypeRepresentation. The equivalent notion used is rather pragmatic, ideally 2 types are equivalent
if you would expect to find them for the same querries while using www.quicktypesearch.com. As there could be several good equivalent notion
candidates, we might use several in the future, though right now we use only one.


For most usefull notions of type-equivalent, finding a normal one is a NP-hard problem, in our case is O(n!) where n represent the number of
diferent type variables; As a work around we use an heuristic to reduce the costs in some scenarios and we compute the computation cost before
actual computing it, tear it down if it is to expensive. 


Right now is still under hevay development (well, as long as I have time...). There are plenty of thing to do, including, to check/proof (or at least
make sure) that the notion of equivalence used is itself an actual notion of equivalence.

