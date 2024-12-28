set_color -u white
echo "Problem set: Random-3-SAT Instances with Controlled Backbone Size (satisfiable)"
set_color normal

fish test/test.sh sat formulas/CBS_k3_n100_m403*/*.cnf
fish test/test.sh sat formulas/CBS_k3_n100_m411*/*.cnf
fish test/test.sh sat formulas/CBS_k3_n100_m418*/*.cnf
fish test/test.sh sat formulas/CBS_k3_n100_m423*/*.cnf
fish test/test.sh sat formulas/CBS_k3_n100_m429*/*.cnf
fish test/test.sh sat formulas/CBS_k3_n100_m435*/*.cnf
fish test/test.sh sat formulas/CBS_k3_n100_m441*/*.cnf
fish test/test.sh sat formulas/CBS_k3_n100_m449*/*.cnf
