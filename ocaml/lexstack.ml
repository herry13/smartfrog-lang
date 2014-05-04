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

(*
 * The next token need to accept an unused dummy lexbuf so that
 * a closure consisting of the function and a lexstack can be passed
 * to the ocamlyacc generated parser.
 *)
let rec get_token ls dummy_lexbuf =
  let token = ls.lexfunc ls.lexbuf in
  match token with
  | Sfparser.INCLUDE filename ->
      ls.stack <- (ls.filename, ls.chan, ls.lexbuf) :: ls.stack;
      ls.filename <- filename;
      ls.chan <- open_in filename;
      ls.lexbuf <- Lexing.from_channel ls.chan;
      get_token ls dummy_lexbuf
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

(* Get the current lexeme. *)
let lexeme ls = Lexing.lexeme ls.lexbuf

(* Get filename, line number and column number of current lexeme. *)
let current_pos ls =
  let pos = Lexing.lexeme_end_p ls.lexbuf in
  let linepos = pos.Lexing.pos_cnum - pos.Lexing.pos_bol - String.length (Lexing.lexeme ls.lexbuf) in
  ls.filename, pos.Lexing.pos_lnum, linepos


