inductive static : Type
| app : static
| com : static → static → static
| fst : static
| lam : static → static
| nam : string → static
| num : int → static
| par : static → static → static
| snd : static
