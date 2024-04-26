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
%type <Problem.t option> problem
%type <int * int> problem_line
%type <int list list> clauses
%type <int list> clause
%type <int list> literals

%%

problem: problem_line clauses EOF {
                        let open Problem in
                        let (v, c) = $1 in
                        Cnf.of_list v c $2 |>
                        Option.map (fun formula ->
                        {
                            formula;
                            config=Config.empty ()
                        })
                      }
       | problem_line EOF {
                        let open Problem in
                        let (v, c) = $1 in
                        Cnf.of_list v c [] |>
                        Option.map (fun formula ->
                        {
                            formula;
                            config=Config.empty ()
                        })
                      }

problem_line: "p" "cnf" INT INT { ($3, $4) }
            | "p" "cnf" "0" INT { (0, $4) }
            | "p" "cnf" "0" "0" { (0, 0) }

clauses: clause clauses { $1 :: $2 }
       | clause { [$1] }
       | clause "%" "0" { [$1] }

clause: literals "0" { $1 }
      | "0" { [] }

literals: INT literals { $1 :: $2 }
        | INT { [$1] }
