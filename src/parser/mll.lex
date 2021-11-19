(* mll.lex
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *
 * ML-Ulex specification for ML Lite.
 *)

%name MLLLex;

%arg (lexErr);

%defs(

    structure T = MLLTokens

  (* some type lex_result is necessitated by ml-ulex *)
    type lex_result = T.token

  (* the depth int ref is used for keeping track of comment depth *)
    val depth = ref 0

  (* starting position of comments/string; used for error reporting *)
    val startPos : Error.pos ref = ref 0

  (* list of string fragments to concatenate *)
    val buf : string list ref = ref []

  (* add a string to the buffer *)
    fun addStr s = (buf := s :: !buf)

  (* make a string from buf *)
    fun mkString () = (T.STRING(String.concat(List.rev(!buf))) before buf := [])

  (* keyword lookup table *)
    local
      val find =
          let val tbl = AtomTable.mkTable (17, Fail "keywords")
              fun ins (id, tok) = AtomTable.insert tbl (Atom.atom id, tok)
          in
              app ins [
                ("case",        T.KW_case),
                ("data",        T.KW_data),
                ("else",        T.KW_else),
                ("end",         T.KW_end),
                ("fun",         T.KW_fun),
                ("if",          T.KW_if),
                ("let",         T.KW_let),
                ("of",          T.KW_of),
                ("then",        T.KW_then)
              ];
              AtomTable.find tbl
          end
    in
  (* return either a keyword token or a LID token *)
    fun idToken id = let
          val ida = Atom.atom id
          in
            case find ida
             of NONE => T.LID ida
              | SOME kw => kw
          end
    end (* local *)
);

%states INITIAL STRING LN_COM BLK_COM;

%let ucLetter = [A-Z];
%let lcLetter = [a-z];
%let letter = {ucLetter}|{lcLetter};
%let dig = [0-9];
%let idchar = {letter}|{dig}|"_";
%let ucId = {ucLetter}{idchar}*;
%let lcId = {lcLetter}{idchar}*;
%let esc = "\\"[nrt\\\"]|"\\"{dig}{dig}{dig};
(* characters that are allowed inside string literals *)
%let sgood = [\032-\126]&[^\"\\];
(* whitespace includes tabs (horizontal and vertical), form feeds, newlines, and returns *)
%let ws = " "|[\t\n\v\f\r];
(* the various end-of-line specifiers *)
%let eol = \n|\r|\r\n;


<INITIAL> "("           => (T.LP);
<INITIAL> ")"           => (T.RP);
<INITIAL> "["           => (T.LB);
<INITIAL> "]"           => (T.RB);
<INITIAL> "{"           => (T.LCB);
<INITIAL> "}"           => (T.RCB);
<INITIAL> ":="          => (T.ASSIGN);
<INITIAL> "||"          => (T.ORELSE);
<INITIAL> "&&"          => (T.ANDALSO);
<INITIAL> "=="          => (T.EQEQ);
<INITIAL> "!="          => (T.NEQ);
<INITIAL> "<="          => (T.LTEQ);
<INITIAL> "<"           => (T.LT);
<INITIAL> "::"          => (T.CONS);
<INITIAL> "^"           => (T.CONCAT);
<INITIAL> "+"           => (T.PLUS);
<INITIAL> "-"           => (T.MINUS);
<INITIAL> "*"           => (T.TIMES);
<INITIAL> "/"           => (T.DIV);
<INITIAL> "%"           => (T.MOD);
<INITIAL> "!"           => (T.DEREF);
<INITIAL> "="           => (T.EQ);
<INITIAL> ","           => (T.COMMA);
<INITIAL> ";"           => (T.SEMI);
<INITIAL> "|"           => (T.BAR);
<INITIAL> "->"          => (T.ARROW);
<INITIAL> "=>"          => (T.DARROW);
<INITIAL> "_"           => (T.WILD);
<INITIAL> {lcId}        => (idToken yytext);
<INITIAL> {ucId}        => (T.UID(Atom.atom yytext));
<INITIAL> {dig}+        => (T.NUMBER(valOf (IntInf.fromString yytext)));
<INITIAL> {ws}          => (skip ());
<INITIAL> "/*"          => (YYBEGIN BLK_COM; startPos := yypos; depth := 1; skip());
<INITIAL> "//"          => (YYBEGIN LN_COM; skip());
<INITIAL> "\""          => (YYBEGIN STRING; startPos := yypos; continue());
<INITIAL>.              => (lexErr((yypos, yypos), [
                                "bad character `", String.toString yytext, "'"
                              ]);
                            skip());
<INITIAL> <<EOF>>       => (T.EOF);

<STRING> {sgood}+       => (addStr yytext; continue());
<STRING> "\\000"        => (lexErr((yypos, yypos), [
                                "illegal escape sequence '\\000' in string"
                              ]);
                            continue());
<STRING> {esc}          => (addStr(valOf(String.fromString yytext)); continue());
<STRING> "\""           => (YYBEGIN INITIAL; mkString());
<STRING> "\\".          => (lexErr((yypos, yypos), [
                                "bad escape character `", String.toString yytext,
                                "' in string literal"
                              ]);
                            continue());
<STRING> {eol}          => (lexErr((!startPos, yypos), [
                                "unclosed string at end of line"
                              ]);
                            YYBEGIN INITIAL; mkString());
<STRING> .              => (lexErr((yypos, yypos), [
                                "bad character `", String.toString yytext,
                                "' in string literal"
                              ]);
                            continue());
<STRING> <<EOF>>        => (lexErr((!startPos, yypos), [
                                "unclosed string at end of file"
                              ]);
                            T.EOF);

<BLK_COM> "/*"          => (depth := !depth + 1;
                            skip());
<BLK_COM> "*/"          => (depth := !depth - 1;
                            if (!depth = 0) then YYBEGIN INITIAL else ();
                            skip ());
<BLK_COM> .|"\n"        => (skip ());
<BLK_COM> <<EOF>>       => (lexErr((!startPos, yypos), [
                                "unclosed comment at end of file"
                              ]);
                            T.EOF);

<LN_COM> {eol}          => (YYBEGIN INITIAL; skip());
<LN_COM> .              => (skip());
<LN_COM> <<EOF>>        => (T.EOF);
