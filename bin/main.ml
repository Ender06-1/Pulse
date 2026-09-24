open Pulse

type arguments = {
  dump_tokens : bool;
  dump_ast : bool;
  dump_typed_ast : bool;
  dump_ir : bool;
  source_path : string;
}

let ( let* ) = Result.bind

let rec parse_args (raw_args : string array) : arguments =
  let aux args argv =
    match argv with
    | "--dump-tokens" -> { args with dump_tokens = true }
    | "--dump-ast" -> { args with dump_ast = true }
    | "--dump-typed-ast" -> { args with dump_typed_ast = true }
    | "--dump-ir" -> { args with dump_ir = true }
    | _ -> { args with source_path = argv }
  in
  Array.fold_left aux
    {
      dump_tokens = false;
      dump_ast = false;
      dump_typed_ast = false;
      dump_ir = false;
      source_path = "";
    }
    raw_args

and lex_all (lexer : Lexer.t) : (Token.t list, Report.t) result =
  let rec aux lexer acc =
    let* tt, lexer = Lexer.next lexer in
    match tt.kind with
    | Token.EOF -> Ok (tt :: acc |> List.rev)
    | _ -> aux lexer (tt :: acc)
  in
  aux lexer []

and dump_tokens (lexer : Lexer.t) : (int, Report.t) result =
  let* tokens = lex_all lexer in
  List.map Token.to_string tokens
  |> String.concat ", " |> Printf.sprintf "[%s]" |> print_endline;
  Ok 0

and dump_ast (program : Ast.t) : unit = Ast.to_string program |> print_endline

and dump_typed_ast (program : Typed_ast.t) : unit =
  Typed_ast.to_string program |> print_endline

and dump_ir (program : Ir.fn list) : unit =
  Ir.string_of_fn_list program |> print_endline

and dump_inst_select (program : string) : unit = print_endline program

and exec_pipeline (args : arguments) : (int, Report.t) result =
  let input = In_channel.with_open_text args.source_path In_channel.input_all in
  let lexer = Lexer.make input args.source_path in
  if args.dump_tokens then dump_tokens lexer
  else
    let* ast = Parser.parse_program lexer in
    if args.dump_ast then (
      dump_ast ast;
      Ok 0)
    else
      let* program = Typed_ast.typecheck ast in
      if args.dump_typed_ast then (
        dump_typed_ast program;
        Ok 0)
      else
        let* cfg = Ir.flatten program in
        if args.dump_ir then (
          dump_ir cfg;
          Ok 0)
        else
          let asm = Codegen_x86_64_linux.codegen cfg in
          let base_name = Filename.remove_extension args.source_path in
          let asm_name = Printf.sprintf "%s.asm" base_name
          and obj_name = Printf.sprintf "%s.o" base_name in
          Out_channel.with_open_text asm_name (fun c ->
              Out_channel.output_string c asm);
          let cmd =
            Printf.sprintf "nasm -felf64 %s && ld %s -o %s" asm_name obj_name
              base_name
          in
          let ret_code = Sys.command cmd in
          Ok ret_code

let () =
  let args = parse_args Sys.argv in
  match exec_pipeline args with
  | Ok ret -> exit ret
  | Error r ->
      Report.to_string r |> prerr_endline;
      exit 1
