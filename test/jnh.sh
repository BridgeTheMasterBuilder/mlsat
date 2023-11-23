set_color -u white
echo "Problem set: JNH - Random SAT instances with variable length clauses (satisfiable)"
set_color normal

fish test/test.sh sat formulas/jnh/jnh{201,204,205,207,209,210,212,213,217,218,220,301,1,7,12,17}.cnf

set_color -u white
echo "Problem set: JNH - Random SAT instances with variable length clauses (unsatisfiable)"
set_color normal

fish test/test.sh unsat formulas/jnh/jnh{10,11,13,14,15,16,18,19,202,203,206,208,20,211,214,215,216,219,2,302,303,304,305,306,307,308,309,310,3,4,5,6,8,9}.cnf
