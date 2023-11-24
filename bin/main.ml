open Cmdliner
open Mlsat
open Data.Config

let parse_args () =
  let dimacs_file =
    Arg.(
      required
      & pos 0 (some string) None
      & info [] ~doc:"DIMACS file" ~docv:"FILE")
  in
  (* let output_assembly = *)
  (*   Arg.(value & flag & info ~doc:"Emit assembly file and exit" [ "s" ]) *)
  (* in *)
  let info = Cmd.info "mlsat" in
  let config = { time_limit = 10.0; base_num_conflicts = 8; grow_factor = 2 } in
  let cmd = Cmd.v info Term.(const Driver.run $ dimacs_file $ const config) in
  exit (Cmd.eval cmd)

let () = parse_args ()
