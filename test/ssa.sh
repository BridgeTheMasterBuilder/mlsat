set_color -u white
echo "Problem set: SSA - Circuit fault analysis: single-stuck-at fault (satisfiable)"
set_color normal

fish test/test.sh sat formulas/ssa/ssa7*.cnf

set_color -u white
echo "Problem set: SSA - Circuit fault analysis: single-stuck-at fault (unsatisfiable)"
set_color normal

fish test/test.sh unsat formulas/ssa/ssa{0,2,6}*.cnf
