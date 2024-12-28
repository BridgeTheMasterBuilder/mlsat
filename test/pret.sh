set_color -u white
echo "Problem set: PRET: Encoded 2-colouring forced to be unsatisfiable (unsatisfiable)"
set_color normal

fish test/test.sh unsat formulas/pret/*.cnf
