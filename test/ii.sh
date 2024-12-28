set_color -u white
echo "Problem set: II: Instances from a problem in inductive inference (satisfiable)"
set_color normal

fish test/test.sh sat formulas/inductive-inference/*.cnf
