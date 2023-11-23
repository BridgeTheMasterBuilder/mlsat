set_color -u white
echo "Problem set: SAT-encoded bounded model checking intances (satisfiable)"
set_color normal

fish test/test.sh sat formulas/bmc/*.cnf
