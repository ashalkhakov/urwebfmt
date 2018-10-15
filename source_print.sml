(* Copyright (c) 2008, Adam Chlipala
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 * - The names of contributors may not be used to endorse or promote products
 *   derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *)

(* Pretty-printing Ur/Web *)

structure FppBasis = FppPrecedenceBasis (FppInitialBasis (FppPlainBasisTypes))
structure Fpp = FinalPrettyPrinter (FppBasis)
structure Render = FppRenderPlainText

structure SourcePrint :> SOURCE_PRINT = struct

open FppBasis Fpp

open Prim
open Source

fun @@ (f, x) = f x
infixr 0 @@

(* if we are doing linebreaks, then it's a newline;
 * otherwise it's just a space
 *)
val softNewline =
    bind spaceWidth (fn w => ifFlat (space w) newline)

fun listSep0 beg sep f ls = let
    fun aux n f ls =
        case ls of
            [] => empty
          | x :: rest =>
            seq [if n = 0 then beg else sep, f x, aux (n+1) f rest]
in
    aux 0 f ls
end

fun listSep sep f ls =
    case ls of
        [] => empty
      | [x] => f x
      | x :: rest => seq [f x, sep, listSep sep f rest]

fun p_t t =
    case t of
        Int n => text (Int64.toString n)
      | Float n => text (Real64.toString n)
      | String (_, s) => hsepTight [text "\"", text (String.toString s), text "\""]
      | Char ch => hsepTight [text "#\"", text (String.toString (String.str ch)), text "\""]

fun p_kind' par (k, _) =
    case k of
        KType => text "Type"
      | KArrow (k1, k2) => hvsep [p_kind' true k1,
                                  text "->",
                                  p_kind k2]
      | KName => text "Name"
      | KRecord k => seq [text "{", p_kind k, text "}"]
      | KUnit => text "Unit"
      | KWild => text "_"
      | KTuple ks => collection (text "(") (text ")") (text "*") (List.map p_kind ks)
      | KVar x => text x
      | KFun (x, k) => hvsep [text x,
                            text "-->",
                            p_kind k]
      | KParen k => seq [text "(", p_kind k, text ")"]

and p_kind k = p_kind' false k

fun p_explicitness e =
    case e of
        Explicit => text "::"
      | Implicit => text ":::"

fun p_con' par (c, _) =
    case c of
        CAnnot (c, k) => hvsep [p_con c, text "::", p_kind k]

      | TFun (t1, t2) => hvsep [p_con' true t1,
                                text "->",
                                p_con t2]
      | TCFun (e, x, k, c) => hvsep [text x,
                                     p_explicitness e,
                                     p_kind k,
                                     text "->",
                                     p_con c]
      | TRecord (CRecord xcs, _) => collection
                                        (text "{") (text "}") (text ",")
                                        (List.map (fn (x, c) => hvsep [p_name x,
                                                                       text ":",
                                                                       p_con c]) xcs)
      | TRecord c => seq [text "$", p_con' true c]
      | TDisjoint (c1, c2, c3) => hvsep [text "[",
                                         p_con c1,
                                         text "~",
                                         p_con c2,
                                         text "]",
                                         text "=>",
                                         p_con c3]

      | CVar (ss, s) => listSep (text ".") text (ss @ [s])
      | CApp (c1, c2) => hvsep [p_con c1,
                                p_con' true c2]
      | CAbs (x, NONE, c) => hvsep [text "fn",
                                    text x,
                                    text "=>",
                                    p_con c]
      | CAbs (x, SOME k, c) => hvsep [text "fn",
                                      text x,
                                      text "::",
                                      p_kind k,
                                      text "=>",
                                      p_con c]


      | CName s => seq [text "#", text s]

      | CRecord xcs => collection (text "[") (text "]") (text ",")
                                  (List.map (fn (x, c) => hvsep [p_con x,
                                                                 text "=",
                                                                 p_con c]) xcs)
      | CConcat (c1, c2) => hvsep [p_con' true c1,
                                   text "++",
                                   p_con c2]
      | CMap => text "map"

      | CUnit => text "()"

      | CWild k => hsep [text "(_",
                        text "::",
                        p_kind k,
                        text ")"]

      | CTuple cs => collection (text "(") (text ")") (text ",") (List.map p_con cs)
      | CProj (c, n) => seq [p_con c,
                             text ".",
                             text (Int.toString n)]

      | CKAbs (x, c) => hvsep [text x,
                               text "==>",
                               p_con c]
      | TKFun (x, c) => hsep [text x,
                              text "-->",
                              p_con c]
      | CParen c => seq [text "(", p_con c, text ")"]

and p_con c = p_con' false c

and p_name (all as (c, _)) =
    case c of
        CName s => text s
      | _ => p_con all

fun p_pat' par (p, _) =
    case p of
        PVar s => text s
      | PPrim p => p_t p
      | PCon (ms, x, NONE) => listSep (text ".") text (ms @ [x])
      | PCon (ms, x, SOME p) => hvsep [listSep (text ".") text (ms @ [x]),
                                       p_pat' true p]
      | PRecord (xps, flex) =>
        let
            val pps = map (fn (x, p) => hsep [text x, text "=", p_pat p]) xps
        in
            bind spaceWidth (fn w =>
                                grouped o hvsep @@ [
                                    text "{",
                                    listSep (seq [text ",", space w]) (fn x => x)
                                            (if flex then
                                                 pps @ [text "..."]
                                             else
                                                 pps),
                text "}"
            ])
        end

      | PAnnot (p, t) => hsep [p_pat p,
                               text ":",
                               p_con t]
      | PCVar (exp, x, k) => hsep [text x, p_explicitness exp, p_kind k]
      | PDisjoint (c1, c2) => hsep [p_con c1, text "~", p_con c2]
      | PKVar k => text k
      | PParen p => seq [text "(", p_pat p, text ")"]

and p_pat x = p_pat' false x

fun p_exp' par (e, _) =
    case e of
        EAnnot (e, t) => hsep [p_exp e, text ":", p_con t]

      | EPrim p => p_t p

      | EUnOp (u, e) => p_unop u e
      | EBinOp (b, e1, e2) => p_binop b e1 e2
      | ENil => text "[]"

      | EVar (ss, s, _) => listSep (text ".") text (ss @ [s])
      | EApp (e1, e2) => hsep [p_exp e1,
                               p_exp' true e2]
      | EAbs (xs, SOME t, e) => bind spaceWidth (fn w => hsep [text "fn",
                                                               listSep (space w) p_pat xs,
                                                               text ":",
                                                               p_con t,
                                                               text "=>",
                                                               p_exp e])
      | EAbs (xs, NONE, e) => bind spaceWidth (fn w => hsep [text "fn",
                                                             listSep (space w) p_pat xs,
                                                             text "=>",
                                                             p_exp e])
      | ECApp (e, c) => hsep [p_exp e,
                              text "[",
                              p_con c,
                              text "]"]
      | ECAbs (exp, x, k, e) => hsep [text "fn",
                                      text x,
                                      p_explicitness exp,
                                      p_kind k,
                                      text "=>",
                                      p_exp e]
      | EDisjoint (c1, c2, e) => hvsep [hsep [p_con c1, text "~", p_con c2, text "=>"],
                                        p_exp e]
      | EDisjointApp e => seq [p_exp e,
                               text "!"]

      | ERecord (xes, flex) =>
        grouped o hvsep @@ [
            text "{",
            listSep (text ",")
                    (fn (x, e) =>
                        hsep [p_name x,
                              text "=",
                              p_exp e]) xes,
            if flex then
                hsep [text ",", text "..."]
            else
                empty,
            text "}"]
      | EField (e, c) => hsep [p_exp' true e,
                               text ".",
                               p_con' true c]
      | EConcat (e1, e2) => hsep [p_exp' true e1,
                                  text "++",
                                  p_exp' true e2]
      | ECut (e, c) => hsep [p_exp' true e,
                             text "--",
                             p_con' true c]
      | ECutMulti (e, c) => hsep [p_exp' true e,
                                  text "---",
                                  p_con' true c]
      | ECase (e, pes) => bind spaceWidth (fn w => hvsep [hsep [text "case", p_exp e, text "of"],
                                                          listSep (seq [space w, text "|", space w])
                                                                  (fn (p, e) => hsep [p_pat p,
                                                                                      text "=>",
                                                                                      p_exp e]) pes])

      | EIf (e1, e2, e3) => bind spaceWidth (fn i =>
                                                grouped (seq [text "if",
                                                              space i,
                                                              p_exp e1,
                                                              softNewline,
                                                              text "then",
                                                              nest 2 (seq [softNewline, p_exp e2]),
                                                              softNewline,
                                                              text "else",
                                                              nest 2 (seq [softNewline, p_exp e3])]))
      | EWild => text "_"

      | ELet (ds, e) => grouped (seq [text "let",
                                      (* welp. sometimes incorrect! tries to conserve horizontal space. *)
                                      nest 2 (listSep0 newline newline p_edecl ds),
                                      newline,
                                      text "in",
                                      nest 2 (seq [newline, p_exp e]),
                                      newline,
                                      text "end"
                                     ])

      | EKAbs (x, e) => hvsep [text x,
                               text "-->",
                               p_exp e]
      | EParen e => grouped o seq @@ [text "(", p_exp e, text ")"]

and p_exp e = p_exp' false e

and p_unop Uneg e = seq [text "-", p_exp e]
and p_binop Borelse e1 e2 = hvsep [p_exp e1, text "||", p_exp e2]
  | p_binop Bandalso e1 e2 = hvsep [p_exp e1, text "&&", p_exp e2]
  | p_binop Bcaret e1 e2 =  hvsep [p_exp e1, text "^", p_exp e2]
  | p_binop Bcons e1 e2 = hvsep [p_exp e1, text "::", p_exp e2]
  | p_binop Beq e1 e2 = hvsep [p_exp e1, text "=", p_exp e2]
  | p_binop Bne e1 e2 = hvsep [p_exp e1, text "<>", p_exp e2]
  | p_binop Bplus e1 e2 = hvsep [p_exp e1, text "+", p_exp e2]
  | p_binop Bminus e1 e2 = hvsep [p_exp e1, text "-", p_exp e2]
  | p_binop Bmult e1 e2 = hvsep [p_exp e1, text "*", p_exp e2]
  | p_binop Bdiv e1 e2 = hvsep [p_exp e1, text "/", p_exp e2]
  | p_binop Bmod e1 e2 = hvsep [p_exp e1, text "%", p_exp e2]
  | p_binop Blt e1 e2 = hvsep [p_exp e1, text "<", p_exp e2]
  | p_binop Ble e1 e2 = hvsep [p_exp e1, text "<=", p_exp e2]
  | p_binop Bgt e1 e2 = hvsep [p_exp e1, text ">", p_exp e2]
  | p_binop Bge e1 e2 = hvsep [p_exp e1, text ">=", p_exp e2]
  | p_binop Bsemi e1 e2 = hvsep [p_exp e1, text ";", p_exp e2]

and p_edecl (d, _) =
  case d of
      EDVal (p, e) => hsep [text "val",
                           p_pat p,
                           text "=",
                           p_exp e]
    | EDValRec vis => bind spaceWidth (fn w => hsep [text "val",
                                                     text "rec",
                                                     listSep (seq [newline, text "and", space w]) p_vali vis])
    | EDFun f => hsep [text "fun", p_fun f]
    | EDFunRec fs => bind spaceWidth (fn w => hsep [text "fun",
                                                    listSep (seq [newline, text "and", space w]) p_fun fs])

and p_funarg p = p_pat p

and p_fun (x, [], co, b) =
    (case co of
         NONE => hsep [text x, text "=", p_exp b]
       | SOME t => hsep [text x, text ":", p_con t, text "=", p_exp b])
  | p_fun (x, ps, co, b) =
    bind spaceWidth (fn w =>
                        case co of
                            NONE =>
                            hsep [text x,
                                  listSep (space w) p_funarg ps,
                                  text "=", p_exp b]
                          | SOME t =>
                            hsep [text x,
                                  listSep (space w) p_funarg ps,
                                  text "=", p_exp b])

and p_vali (x, ps, co, e) =
    bind spaceWidth (fn w =>
    case co of
        NONE => hsep [text x,
                     listSep (space w) p_pat ps,
                     text "=",
                     p_exp e]
      | SOME t => hsep [text x,
                       listSep (space w) p_pat ps,
                       text ":",
                       p_con t,
                       text "=",
                       p_exp e])


fun p_datatype (x, xs, cons) =
    bind spaceWidth (fn w =>
                        hsep [text x,
                              listSep (space w) (fn x => text x) xs,
                              text "=",
                              listSep (hsep [space w, text "|", space w])
                                      (fn (x, NONE) => text x
                                      | (x, SOME t) => hsep [text x, text "of", p_con t])
                                      cons])

fun p_sgn_item (sgi, _) =
    case sgi of
        SgiConAbs (x, k) => hsep [text "con",
                                 text x,
                                 text "::",
                                 p_kind k]
      | SgiCon (x, NONE, c) => hsep [text "con",
                                    text x,
                                    text "=",
                                    p_con c]
      | SgiCon (x, SOME k, c) => hsep [text "con",
                                      text x,
                                      text "::",
                                      p_kind k,
                                      text "=",
                                      p_con c]
      | SgiDatatype x => bind spaceWidth (fn w =>
                                             hsep [text "datatype",
                                                   listSep (seq [space w, text "and", space w]) p_datatype x])
      | SgiDatatypeImp (x, ms, x') =>
        hsep [text "datatype",
             text x,
             text "=",
             text "datatype",
             listSep (text ".") text (ms @ [x'])]
      | SgiVal (x, c) => hsep [text "val",
                              text x,
                              text ":",
                              p_con c]
      | SgiTable (x, c, pe, ce) => hsep [text "table",
                                        text x,
                                        text ":",
                                        p_con c,
                                        text "keys",
                                        p_exp pe,
                                        text "constraints",
                                        p_exp ce]
      | SgiStr (x, sgn) => hsep [text "structure",
                                text x,
                                text ":",
                                p_sgn sgn]
      | SgiSgn (x, sgn) => hsep [text "signature",
                                text x,
                                text "=",
                                p_sgn sgn]
      | SgiInclude sgn => hsep [text "include",
                                p_sgn sgn]
      | SgiConstraint (c1, c2) => hsep [text "constraint",
                                       p_con c1,
                                       text "~",
                                       p_con c2]
      | SgiClassAbs (x, k) => hsep [text "class",
                                   text x,
                                   text "::",
                                   p_kind k]
      | SgiClass (x, k, c) => hsep [text "class",
                                   text x,
                                   text "::",
                                   p_kind k,
                                   text "=",
                                   p_con c]

and p_sgn (sgn, _) =
    case sgn of
        SgnConst sgis => seq [text "sig",
                              newline,
                              nest 2 (listSep newline p_sgn_item sgis),
                              newline,
                              text "end"]
      | SgnVar x => text x
      | SgnFun (x, sgn, sgn') => hsep [text "functor",
                                      text "(",
                                      text x,
                                      text ":",
                                      p_sgn sgn,
                                      text ")",
                                      text ":",
                                      p_sgn sgn']
      | SgnWhere (sgn, ms, x, c) => hsep [p_sgn sgn,
					 text "where",
					 text "con",
					 listSep (text ".")
						 text (ms @ [x]),
					 text x,
					 text "=",
					 p_con c]
      | SgnProj (m, ms, x) => listSep (text ".") text (m :: ms @ [x])



fun p_decl ((d, _) : decl) =
    case d of
        DCon (x, NONE, c) => bind spaceWidth (fn w => seq [hsep [text "con", text x, text "="], space w, p_con c])
      | DCon (x, SOME k, c) => bind spaceWidth (fn w => seq [hsep [text "con", text x, text "::", p_kind k, text "="], space w, p_con c])
      | DDatatype x => bind spaceWidth (fn w => seq [hsep [text "datatype"], space w,
                                                     listSep (seq [newline, text "and", space w]) p_datatype x])
      | DDatatypeImp (x, ms, x') =>
        seq [hsep [text "datatype", text x, text "=", text "datatype"],
             listSep (text ".") text (ms @ [x'])]
      | DVal (p, e) => hsep [text "val", p_pat p, text "=", hvsep [p_exp e]]
      | DValRec vis => bind spaceWidth (fn w => hsep [text "val", text "rec", listSep (seq [newline, text "and", space w]) p_vali vis])
      | DFun f => hsep [text "fun", p_fun f]
      | DFunRec fs => bind spaceWidth (fn w => hsep [text "fun",
                                                     listSep (seq [newline, text "and", space w]) p_fun fs])

      | DSgn (x, sgn) => hsep [text "signature", text x, text "=", p_sgn sgn]
      | DStr (x, NONE, _, str, _) => hsep [text "structure",
                                          text x,
                                          text "=",
                                          p_str str]
      | DStr (x, SOME sgn, _, str, _) => hsep [text "structure",
                                              text x,
                                              text ":",
                                              p_sgn sgn,
                                              text "=",
                                              p_str str]
      | DFfiStr (x, sgn, _) => hsep [text "extern",
                                    text "structure",
                                    text x,
                                    text ":",
                                    p_sgn sgn]
      | DOpen (m, ms) => hsep [text "open",
                              listSep (text ".") text (m :: ms)]
      | DConstraint (c1, c2) => hsep [text "constraint",
                                     p_con c1,
                                     text "~",
                                     p_con c2]
      | DOpenConstraints (m, ms) => hsep [text "open",
                                         text "constraints",
                                         listSep (text ".") text (m :: ms)]

      | DExport str => hsep [text "export",
                            p_str str]
      | DTable (x, c, pe, ce) => hsep [text "table",
                                      text x,
                                      text ":",
                                      p_con c,
                                      text "keys",
                                      p_exp pe,
                                      text "constraints",
                                      p_exp ce]
      | DSequence x => hsep [text "sequence", text x]
      | DView (x, e) => hsep [text "view",
                              text x,
                              text "=",
                              p_exp e]

      | DDatabase s => hsep [text "database",
                            text s]

      | DCookie (x, c) => hsep [text "cookie",
                                text x,
                                text ":",
                                p_con c]
      | DStyle x => hsep [text "style", text x]
      | DTask (e1, e2) => hsep [text "task",
                                p_exp e1,
                                text "=",
                                p_exp e2]
      | DPolicy e1 => hsep [text "policy",
                            p_exp e1]
      | DOnError _ => text "ONERROR"
      | DFfi _ => text "FFI"

and p_str (str, _) =
    case str of
        StrConst ds => seq [text "struct",
                            newline,
                            nest 2 (listSep newline p_decl ds),
                            newline,
                            text "end"]
      | StrVar x => text x
      | StrProj (str, x) => seq [p_str str,
                                 text ".",
                                 text x]
      | StrFun (x, sgn, NONE, str) => hsep [text "functor",
                                           text "(",
                                           text x,
                                           text ":",
                                           p_sgn sgn,
                                           text ")",
                                           text "=>",
                                           newline,
                                           p_str str]
      | StrFun (x, sgn, SOME sgn', str) => hsep [text "functor",
                                                text "(",
                                                text x,
                                                text ":",
                                                p_sgn sgn,
                                                text ")",
                                                text ":",
                                                p_sgn sgn',
                                                text "=>",
                                                newline,
                                                p_str str]
      | StrApp (str1, str2) => hsep [p_str str1,
                                    text "(",
                                    p_str str2,
                                    text ")"]

val p_file = listSep newline p_decl

local

    val initialEnv =
        {maxWidth = 80,
         maxRibbon = 60,
         layout = FppTypes.BREAK,
         failure = FppTypes.CANT_FAIL,
         nesting = 0,
         formatting = (),
         formatAnn = fn _ => ()}

in

fun execPP (m : unit m) =
    #output @@ m emptyPrecEnv initialEnv {curLine = [], maxWidthSeen = 0}

end

fun renderToStream (strm, v) =
    Render.render strm (execPP v)

end
