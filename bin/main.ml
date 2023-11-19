open Cmdliner
open Mlsat

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
  let cmd = Cmd.v info Term.(const Driver.run $ dimacs_file) in
  exit (Cmd.eval cmd)

let () = parse_args ()
