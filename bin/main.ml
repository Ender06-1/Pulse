let ( let* ) = Result.bind

let compile (path : string) : (int, string) result =
  let input =
    In_channel.with_open_text (Array.get Sys.argv 1) In_channel.input_all
  in
  let lexer = Pulse.Lexer.make input in
  let* program, _ = Pulse.Parser.parse_program lexer in
  let* ir = Pulse.Ir.flatten program in
  let asm = Pulse.Codegen.codegen ir in
  Out_channel.with_open_text "main.pulse.asm" (fun c ->
      Out_channel.output_string c asm);
  Ok
    (Sys.command
       "nasm -felf64 main.pulse.asm && ld main.pulse.o -o main.pulse.exe")

let () =
  match compile Sys.argv.(1) with
  | Ok ret_code -> exit ret_code
  | Error e ->
      print_endline e;
      exit 1
