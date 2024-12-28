set_color -u white
echo "Problem set: HANOI: SAT-encoding of Towers of Hanoi (satisfiable)"
set_color normal

fish test/test.sh sat formulas/hanoi/*.cnf
