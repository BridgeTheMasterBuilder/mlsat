%{
    open Data
%}

%token CNF "cnf"
%token END "0"
%token EOF
%token <int> INT
%token P "p"
%token PERCENT "%"

%start problem
%type <Problem.t> problem

%%

problem: problem_line clauses EOF {
                        let (v, c) = $1 in
                        ({
                          formula = Cnf.of_list $2;
                          time_limit = 60.0;
                          base_num_conflicts = 5;
                          grow_factor = 2;
                        } : Problem.t)
                      }

problem_line: "p" "cnf" INT INT { ($3, $4) }

clauses: clause clauses { $1 :: $2 }
       | clause { [$1] }
       | clause "0" { [$1] }
       | clause "%" "0" { [$1] }

clause: literals "0" { $1 }

literals: INT literals { $1 :: $2 }
        | INT { [$1] }
