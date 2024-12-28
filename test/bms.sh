set_color -u white
echo "Problem set: Backbone-minimal Sub-instances (satisfiable)"
set_color normal

fish test/test.sh sat formulas/BMS_k3_n100_m429/*.cnf
