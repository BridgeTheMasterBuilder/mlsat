set_color -u white
echo "Problem set: Random-3-SAT Instances with Controlled Backbone Size (satisfiable)"
set_color normal

fish test/test.sh sat formulas/CBS*/*.cnf
