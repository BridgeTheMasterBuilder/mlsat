open Cmdliner
open Mlsat
open Data.Config

let parse_args () =
  let dimacs_file =
    Arg.(
      required
      & pos 0 (some string) None
      & info [] ~doc:"DIMACS file" ~docv:"FILE" )
  in
  let time_limit =
    Arg.(
      value & opt float 60.0
      & info ["t"; "time-limit"] ~docv:"SECS"
          ~doc:"Time limit for determining satisfiability of a formula" )
  in
  let verbose =
    Arg.(
      value & flag
      & info ["v"; "verbose"] ~doc:"Whether to print debug information" )
  in
  (* let no_validate = *)
  (*   Arg.( *)
  (*     value & flag *)
  (*     & info [ "n"; "no-validate" ] ~doc:"Don't validate DIMACS file") *)
  (* in *)
  let base_num_conflicts =
    Arg.(
      value & opt int 256
      & info
          ["c"; "base-num-conflicts"]
          ~docv:"NUM"
          ~doc:"Base number of allowed conflicts before triggering a restart" )
  in
  let grow_factor =
    Arg.(
      value & opt int 2
      & info ["g"; "grow-factor"] ~docv:"N"
          ~doc:"Growth factor of allowed conflicts between successive restarts" )
  in
  let emit_proof =
    Arg.(
      value
      & opt ~vopt:(Some "proof.out") (some string) None
      & info ["p"; "emit-proof"] ~docv:"FILENAME"
          ~doc:"Produce a proof of unsatisfiability" )
  in
  let max_learned =
    Arg.(
      value & opt int 8192
      & info ["l"; "max-learned"] ~docv:"NUM"
          ~doc:
            "Maximum number of clause that can be learned before triggering \
             deletion" )
  in
  let decay_interval =
    Arg.(
      value & opt int 1000
      & info ["i"; "decay-interval"] ~docv:"NUM"
          ~doc:"Number of conflicts between decaying literal activity" )
  in
  let decay_factor =
    Arg.(
      value & opt float 0.99
      & info ["f"; "decay-factor"] ~docv:"NUM"
          ~doc:"Multiplication factor to use when decaying literal activity" )
  in
  let config =
    Term.(
      const
        (fun
          time_limit
          verbose
          (* no_validate *) base_num_conflicts
          grow_factor
          emit_proof
          max_learned
          decay_interval
          decay_factor
        ->
          { time_limit
          ; verbose
          ; (* no_validate; *)
            base_num_conflicts
          ; grow_factor
          ; emit_proof
          ; max_learned
          ; decay_interval
          ; decay_factor } )
      $ time_limit $ verbose
      (* $ no_validate *)
      $ base_num_conflicts
      $ grow_factor $ emit_proof $ max_learned $ decay_interval $ decay_factor )
  in
  let info = Cmd.info "mlsat" in
  let cmd = Cmd.v info Term.(const Driver.run $ dimacs_file $ config) in
  exit (Cmd.eval cmd)

let () = parse_args ()
