set_color -u white
echo "Problem set: SAT Competition Beijing (satisfiable?)"
set_color normal

fish test/test.sh sat formulas/Beijing/*.cnf
