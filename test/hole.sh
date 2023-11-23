set_color -u white
echo "Problem set: PHOLE: Pigeon hole problem (unsatisfiable)"
set_color normal

fish test/test.sh unsat formulas/pigeon-hole/*.cnf
