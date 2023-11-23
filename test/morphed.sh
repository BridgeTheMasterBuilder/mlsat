set_color -u white
echo "Problem set: \"Morphed\" Graph Colouring, 5 colourable (satisfiable)"
set_color normal

# fish test/test.sh sat formulas/sw*/*.cnf
fish test/test.sh sat formulas/sw*lp{0,1,2,3,8}*/*.cnf
