%{
    open Data
    open Common

    let variable_set = ref IntSet.empty
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
                        let (v, c) = $1 in
                        let v' = IntSet.cardinal !variable_set in
                        let c' = List.length $2 in
                        if v <> v' || c <> c' then
                            failwith "Malformed DIMACS file"
                        else
                            ({
                            formula = Cnf.of_list $2;
                            config=Config.empty
                            } : Problem.t)
                      }

problem_line: "p" "cnf" INT INT { ($3, $4) }

clauses: clause clauses { $1 :: $2 }
       | clause { [$1] }
       | clause "0" { [$1] }
       | clause "%" "0" { [$1] }

clause: literals "0" { $1 }

literals: INT literals { variable_set := IntSet.add (abs $1) !variable_set; $1 :: $2 }
        | INT { variable_set := IntSet.add (abs $1) !variable_set; [$1] }
