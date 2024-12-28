set_color -u white
echo "Problem set: BF: Circuit fault analysis: bridge fault (unsatisfiable)"
set_color normal

fish test/test.sh unsat formulas/bf/*.cnf
