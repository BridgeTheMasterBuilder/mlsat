set_color -u white
echo "Problem set: Basic edge cases"
set_color normal

fish test/test.sh sat formulas/basic/empty-formula.cnf
fish test/test.sh unsat formulas/basic/empty-clause.cnf
fish test/test.sh sat formulas/basic/p-or-not-p.cnf
fish test/test.sh unsat formulas/basic/p-and-not-p.cnf
