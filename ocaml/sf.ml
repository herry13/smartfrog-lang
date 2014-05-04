(* The Lexstack type. *)
type 'a t =
  {
    mutable stack : (string * in_channel * Lexing.lexbuf) list;
    mutable filename : string;
    mutable chan : in_channel;
    mutable lexbuf : Lexing.lexbuf;
    lexfunc : Lexing.lexbuf -> 'a;
  }

(*
 * Create a lexstack with an initial top level filename and the lexer function
 *)
let create top_filename lexer_function =
  let chan = open_in top_filename in
  {
    stack = [];
    filename = top_filename;
    chan = chan;
    lexbuf = Lexing.from_channel chan;
    lexfunc = lexer_function
  }

(* Get the current lexeme. *)
let lexeme ls = Lexing.lexeme ls.lexbuf

(* Get filename, line number and column number of current lexeme. *)
let current_pos ls =
  let pos = Lexing.lexeme_end_p ls.lexbuf in
  let linepos = pos.Lexing.pos_cnum - pos.Lexing.pos_bol - String.length (Lexing.lexeme ls.lexbuf) in
  ls.filename, pos.Lexing.pos_lnum, linepos

let dummy_lexbuf = Lexing.from_string ""

(*
 * The next token need to accept an unused dummy lexbuf so that
 * a closure consisting of the function and a lexstack can be passed
 * to the ocamlyacc generated parser.
 *)
let rec get_token ls dummy_lexbuf =
  let token = ls.lexfunc ls.lexbuf in
  match token with
  | Sfparser.INCLUDE file -> (* parse included file *)
      (*** below is the C-style include (the included file does not have to be legal statements) ***)
      (*
      ls.stack <- (ls.filename, ls.chan, ls.lexbuf) :: ls.stack;
      ls.filename <- file;
      ls.chan <- open_in file;
      ls.lexbuf <- Lexing.from_channel ls.chan;
      get_token ls dummy_lexbuf
      *)

      (** SF-style include (the included file must be legal statements) **)
      Sfparser.SF_INCLUDE (
        fun ns s -> let lexstack = create file Sflexer.token in
                    try 
                      Sfparser.included (get_token lexstack) dummy_lexbuf ns s
                    with e -> check_error e lexstack
      )
  | Sfparser.EOF ->
      ( match ls.stack with
        | [] -> Sfparser.EOF
        | (fn, ch, lb) :: tail ->
            ls.filename <- fn;
            ls.chan <- ch;
            ls.stack <- tail;
            ls.lexbuf <- lb;
            get_token ls dummy_lexbuf
      )
  | _ -> token

and check_error e lexstack =
  match e with
  | Parsing.Parse_error ->
      let fname, lnum, lpos = current_pos lexstack in
      let errstr = Printf.sprintf
        "\n\nFile '%s' line %d, column %d : current token is '%s'.\n\n"
        fname lnum lpos (lexeme lexstack) in
      raise (Failure errstr)
  | e -> raise e

(* parse main file *)
and parse file =
  let lexstack = create file Sflexer.token in
  try 
    Sfparser.sf (get_token lexstack) dummy_lexbuf
  with e -> check_error e lexstack
