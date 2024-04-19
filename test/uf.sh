set_color -u white
echo "Problem set: Uniform Random-3-SAT (satisfiable, max. 150 variables)"
set_color normal

fish test/test.sh sat formulas/uf{20,50,75,100,125,150}-*/*.cnf

set_color -u white
echo "Problem set: Uniform Random-3-SAT (unsatisfiable, max. 125 variables)"
set_color normal

fish test/test.sh unsat formulas/uuf{20,50,75,100,125,150}-*/*.cnf
