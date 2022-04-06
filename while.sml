structure AST =
struct
type id = string

exception Error
(*datatype binop = Add | Sub | Mul | Equal | Great | Small | And | Or | Xor | Implies 
*)
datatype Empty  = empty of unit

datatype Type = INT | BOOL
datatype RelOp = LT | GT | LEQ | GEQ | NEQ | EQ | AND | OR
datatype Addop = PLUS | MINUS 
datatype MulOP = MUL | DIV | MOD
datatype Bool = TT | FF 
datatype UniOp = NOT | TILDE 
type variable = id
datatype commandseq =  empty_cmd_seq of unit | SEQ of (command) list 
and command = SET of id*exp | read of id | write of exp | ITE of exp * commandseq * commandseq | WH of exp * commandseq 
and exp = bool of Bool | num of int | Addexp of exp * Addop * exp | mulexp of exp * MulOP * exp | Relexp of exp * RelOp * exp | Uniexp of exp * UniOp | Paren of exp
            | iden of variable | plus_exp of exp

type variablelist = (variable) list
datatype mergeType = type_var of variablelist * Type
datatype declaration = decl of variable * mergeType
datatype DEC = DEC of variablelist * Type * DEC | dec_empty of unit

fun empty_command_sequence() =
    let 
        val a : command list = []
    in a 
end    

fun string_to_iden(a : string) = 
    let 
        val b : id = a 
    in b 
end


fun com_seq_to_list(a: command, b :commandseq) =
    let 
        val SEQ(z) = b
        val c : command list = [a] @ z
    in c 
end

fun com_seq_to_list1(b :commandseq) =
    let 
        val SEQ(z) = b
        val c : command list = z
    in c 
end


fun com_list_to_seq(a : command list) = 
    let
        val  b : commandseq = SEQ(a)
    in b 
end

fun id_to_var (x : id) = 
    let
        val y : variable = x 
    in y 
end

fun var_to_id (x : variable) = 
    let
        val y : id = x 
    in y 
end


datatype block = BLK of DEC * commandseq
datatype program = PROG of id * block

end