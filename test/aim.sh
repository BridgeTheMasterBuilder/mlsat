set_color -u white
echo "Problem set: AIM - Artificially generated Random-3-SAT (satisfiable)"
set_color normal

fish test/test.sh sat formulas/aim/aim-*yes*.cnf

set_color -u white
echo "Problem set: AIM - Artificially generated Random-3-SAT (unsatisfiable)"
set_color normal

fish test/test.sh unsat formulas/aim/aim-*no*.cnf
