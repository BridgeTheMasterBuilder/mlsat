set_color -u white
echo "Problem set: PARITY - Instances for problem in learning the parity function (satisfiable)"
set_color normal

fish test/test.sh sat formulas/parity/*.cnf
