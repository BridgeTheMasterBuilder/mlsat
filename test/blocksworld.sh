set_color -u white
echo "Problem set: Planning, blocksworld (satisfiable)"
set_color normal

fish test/test.sh sat formulas/blocksworld/*.cnf
