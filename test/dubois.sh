set_color -u white
echo "Problem set: Dubois - Randomly generated SAT instances (unsatisfiable)"
set_color normal

fish test/test.sh unsat formulas/dubois/*.cnf
