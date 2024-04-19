set_color -u white
echo "Problem set: \"Flat\" Graph Colouring, 3 colourable (satisfiable)"
set_color normal

fish test/test.sh sat formulas/flat*/*.cnf
