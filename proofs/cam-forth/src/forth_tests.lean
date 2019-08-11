import forth
import utils

open except
open forth_program
open forth_vm
open forth_word

def print_eight : forth_program := new [lit 8, print]

#reduce run 2 print_eight
-- TODO
-- example : run 2 print_eight = ok (some [8]) := rfl
