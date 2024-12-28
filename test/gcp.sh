set_color -u white
echo "Problem set: GCP - Large SAT-encoded Graph Colouring problems (satisfiable)"
set_color normal

fish test/test.sh unsat formulas/gcp-large/*.cnf
