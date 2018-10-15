#!/bin/bash

URWEBFMT=../urwebfmt

$URWEBFMT $1 > $1.out
diff $1.ok $1.out

# two things
# first, the two programs before/after have same types
#  urweb -tc SOMETHING SOMETHING GIVE ME THE TYPE
# second, the latter program conforms to the new style
#  that is what we check already
