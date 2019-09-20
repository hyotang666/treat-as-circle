# TREAT-AS-CIRCLE 0.0.0 - Tiny library for treating sequence as circle.

* Current lisp world
List is very flexible.
It can refer themselves.
The list which refers themselves is called 'circle list'.

* Issues
When you use circle list, you need to bear some losses.
0. You may need to make circle list with SETF, unless it is made as literal with #n= and #n#.
1. You need to assign `*print-circle*` T.
2. After making circle list, you may not be able to use some useful sequence function e.g. length, find, remove, position etc...
3. Even if implementation allows you to use sequence function for circle list, you can get into infinite loop easily.
4. You never modify its length.

* Proposal
Treat-as-circle solves any issues above.
Like its name indicates, treat-as-circle treats sequence as circle.
Yes, you can use any sequences includes bit vector as circle.
(note - list must be proper.)

## Usage
```lisp
(defvar list '(a b c))
=> LIST
(nth-as-circle 0 list)
=> A
(nth-as-circle 3 list)
=> A
(nth-as-circle -1 list)
=> C
```
Treat-as-circle provides the symbols named XXX-as-circle.
XXX is nth, elt, char, schar, aref, svref, bit, or sbit.
API is inherit from CL's.
(i.e. nth-as-circle accepts index as first argument, but others.)

## For hackers
XXX-as-circle call CL:LENGTH each time.
It may be over head, especially called in loop.
In such cases, you can use internal %XXX-as-circle with `*LENGTH*` combination.
```lisp
(let*((list '(a b c))
      (*LENGTH* (length list)))
  (loop :for n :below 10
        :collect (%NTH-AS-CIRCLE n list)))
=> (A B C A B C A B C A)
```
NOTE - aref-as-circle, and bit-as-circle does not have internal one.
## From developer

* Product's goal - Already?
* License - MIT
* Developped with - CLISP
* Tested with - 

