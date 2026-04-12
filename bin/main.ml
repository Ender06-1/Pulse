open Pulse

let () =
  if Array.length Sys.argv <> 2 then (
    prerr_endline "invalid number of arguments";
    exit 1)
  else
    let tree =
      In_channel.with_open_text Sys.argv.(1) In_channel.input_all
      |> Pulse.Lexer.make Sys.argv.(1)
      |> Parser.parse
    in
    match tree with
    | Error e ->
        prerr_endline e;
        exit 1
    | Ok tree ->
        Out_channel.with_open_text "main.asm" (fun oc ->
            Codegen.gen tree |> Out_channel.output_string oc);
        Sys.command "fasm main.asm" |> exit
