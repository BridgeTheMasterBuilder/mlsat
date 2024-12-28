set_color -u white
echo "Problem set: LRAN - Large Random-3-SAT instances (satisfiable)"
set_color normal

fish test/test.sh sat formulas/f/*.cnf
