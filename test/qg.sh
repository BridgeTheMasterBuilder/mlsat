set_color -u white
echo "Problem set: SAT-encoded Quasigroup (or Latin square) instances (satisfiable)"
set_color normal

fish test/test.sh sat formulas/QG/qg{1,2,3-08,4-09,5-11,6-09,7-09,7-13}*.cnf

set_color -u white
echo "Problem set: SAT-encoded Quasigroup (or Latin square) instances (unsatisfiable)"
set_color normal

fish test/test.sh unsat formulas/QG/qg{3-09,4-08,5-09,5-10,5-12,5-13,6-10,6-11,6-12,7-10,7-11,7-12}*.cnf
