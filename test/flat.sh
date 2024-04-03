set_color -u white
echo "Problem set: \"Flat\" Graph Colouring, 3 colourable (satisfiable)"
set_color normal

# fish test/test.sh sat formulas/flat{30,50,75,100,125,150,175}*/*.cnf
fish test/test.sh sat formulas/flat*/*.cnf
