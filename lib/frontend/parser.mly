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
%type <int * int> problem_line
%type <int list list> clauses
%type <int list> clause
%type <int list> literals

%%

problem: problem_line clauses EOF {
                        let open Problem in
                        let (v, c) = $1 in
                        {
                            formula = Cnf.of_list v c $2;
                            config=Config.empty
                        }
                      }
       | problem_line EOF {
                        let open Problem in
                        let (v, c) = $1 in
                        {
                            formula = Cnf.of_list v c [];
                            config=Config.empty
                        }
                      }

problem_line: "p" "cnf" INT INT { ($3, $4) }
            | "p" "cnf" END INT { (0, $4) }
            | "p" "cnf" END END { (0, 0) }

clauses: clause clauses { $1 :: $2 }
       | clause { [$1] }
       | clause "%" "0" { [$1] }
       | "0" { [] }

clause: literals "0" { $1 }

literals: INT literals { $1 :: $2 }
        | INT { [$1] }
