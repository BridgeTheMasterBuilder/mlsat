set_color -u white
echo "Problem set: Random-3-SAT (satisfiable)"
set_color normal

fish test/test.sh sat formulas/RTI_k3_n100_m429/*.cnf
