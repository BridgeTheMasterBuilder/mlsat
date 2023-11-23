set_color -u white
echo "Problem set: All Interval Series (satisfiable)"
set_color normal

fish test/test.sh sat formulas/ais/*.cnf
