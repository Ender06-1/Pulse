let () =
  let input =
    In_channel.with_open_text (Array.get Sys.argv 1) In_channel.input_all
  in
  let lexer = Pulse.Lexer.make input in
  let ret_code =
    Result.fold
      ~ok:(fun (tree, _) ->
        let ir = Pulse.Ir.flatten tree in
        let asm = Pulse.Codegen.codegen ir in
        let str_code =
          List.map Pulse.Codegen.string_of_instruction asm |> String.concat "\n"
        in
        let program =
          String.cat
            (String.cat Pulse.Codegen.prologue str_code)
            Pulse.Codegen.epilogue
        in
        Out_channel.with_open_text "main.pulse.asm" (fun c ->
            Out_channel.output_string c program);
        Sys.command
          "nasm -felf64 main.pulse.asm && ld main.pulse.o -o main.pulse.exe")
      ~error:(fun e ->
        print_endline e;
        1)
      (Pulse.Parser.parse_program lexer)
  in
  exit ret_code
