# [Function] ELT-AS-CIRCLE
# [Function] NTH-AS-CIRCLE
# [Function] AREF-AS-CIRCLE
# [Function] CHAR-AS-CIRCLE
# [Function] SCHAR-AS-CIRCLE
# [Function] SVREF-AS-CIRCLE
# [Function] BIT-AS-CIRCLE
# [Function] SBIT-AS-CIRCLE

## Syntax:

(basic-operator SEQUENCE index) => result
(NTH-AS-CIRCLE index LIST) => result
(array-operator array index+) => result

## Arguments and Values:

basic-operator := [ ELT-AS-CIRCLE | CHAR-AS-CIRCLE | SCHAR-AS-CIRCLE | SVREF-AS-CIRCLE ]

index := (AND INTEGER (NOT (SATISFIES MINUSP)))

result := ANY LISP OBJECT

array-operator := [ AREF-AS-CIRCLE | BIT-AS-CIRCLE | SBIT-AS-CIRCLE ]

## Description:

System TREAT-AS-CIRCLE is intended to kill circle list out from Common Lisp world.
All operator is SETFable.

### Profits:

* Don't need to make circle list actually.
* Don't need to set `*PRINT-CIRCLE*` T.
* We can use any sequence builtin command such as FIND, REMOVE, etc...
* Not only LIST, we can use STRING, VECTOR, ARRAY, etc...
* Negative INTEGER is acceptable for index.

### Characteristics:

* NTH-AS-CIRCLE

Parameter order is inherited from Common Lisp's NTH.
INDEX comes first.

* AREF-AS-CIRCLE BIT-AS-CIRCLE SBIT-AS-CIRCLE

Treating any dimensions as circle.

* index

When negative integer is specified for index, -1 is last element of sequence.

### for hackers:
Main APIs computes sequence's length at first.
If, e.g., nth-as-circle called inside loop, and sequence never change its length, computing its length becomes unignorable overhead.
In such case, you can use internal symbols like below.
```lisp
(let((vector(make-array 1000))
     (treat-as-circle::*length*(length vector)))
  (loop :for n :below 5000
        :do (setf (treat-as-circle::%svref-as-circle vector n)
	          (random 1000)))
  vector)
```

## Examples:
```lisp
(nth-as-circle 0 '(a b c)) ; => A
(nth-as-circle 3 '(a b c)) ; => A
(nth-as-circle -1 '(a b c)) ; => C
(nth-as-circle -3 '(a b c)) ; => A
```
## See Also:

[Common Lisp]
ELT NTH AREF SVREF CHAR SCHAR BIT SBIT `*PRINT-CIRCLE*`
