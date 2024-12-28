set_color -u white
echo "Problem set: Planning, logistics (satisfiable)"
set_color normal

fish test/test.sh sat formulas/logistics/*.cnf
