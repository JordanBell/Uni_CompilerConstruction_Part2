open Parser_types
open Parser_printer

type eval_result = 
	| Int of int
	| Bool of bool
	| String of string
	| Unit

let string_of_eval_result er = match er with
	| Int (i) -> string_of_int i
	| Bool (b) -> if b then "true" else "false"
	| String (s) -> s
	| Unit -> "Unit"

let fun_of_opcode_int = function
	| Plus -> (fun x y -> x + y)
	| Minus -> (fun x y -> x - y)
	| Times -> (fun x y -> x * y)
	| Divide -> (fun x y -> x / y)
	| _ -> failwith "Mismatch: Cannot use operator on int types"

let fun_of_opcode_comp = function
	| Geq -> (fun x y -> x >= y)
	| Leq -> (fun x y -> x <= y)
	| Equal -> (fun x y -> x == y)
	| Noteq -> (fun x y -> x != y)
	| _ -> failwith "Mismatch: Cannot use operator on bool types"

let fun_of_opcode_bool = function	
	| And -> (fun x y -> x && y)
	| Or -> (fun x y -> x || y)
	| _ -> failwith "Mismatch: Cannot use operator on bool types"

let rec evaluate_expression store e = match e with
	| Const (i) -> Int (i)
	| Operator (opcode, e, f) -> evaluate_operator store opcode e f
	| Operator_unary (opcode, e) -> evaluate_operator_unary store opcode e
	| If (b, e, f) -> failwith "Expression type not implemented: If" 
	| Seq (e, f) -> failwith "Expression type not implemented: Seq"
	| Asg (e, f) -> failwith "Expression type not implemented: Asg"
	| While (e, f) -> failwith "Expression type not implemented: While"
	| Readint -> failwith "Expression type not implemented: Readint"
	| Printint (e) -> failwith "Expression type not implemented: Printint"
	| Application (e, f) -> failwith "Expression type not implemented: Application"
	| Identifier (str) -> failwith "Expression type not implemented: Identifier"
	| New (str, e, f) -> failwith "Expression type not implemented: New"
	| Deref (e) -> failwith "Expression type not implemented: Deref"
	| Scope (e) -> evaluate_expression store e
	| Empty -> Unit
and evaluate_operator store opcode e f = 
	(* Get the binary operator for the opcode *)

	(* Evaluate the sub-expressions *)
	let result_e = evaluate_expression store e in 
	let result_f = evaluate_expression store f in
	match result_e, result_f with
		(* Match the eval_results with appropriate opcodes *)
		| Int (rei), Int (rfi) -> 
			(match opcode with
				| Plus | Minus | Times | Divide -> 
					let int_result = (fun_of_opcode_int opcode) rei rfi in
					Int (int_result)
				| Geq | Leq | Equal | Noteq -> 
					let bool_result = (fun_of_opcode_comp opcode) rei rfi in
					Bool (bool_result)
				| _ -> failwith "Cannot perform non-int operations on int values")
		| Bool (rei), Bool (rfi) -> 
			(let opcode_as_fun = fun_of_opcode_bool opcode in
			match opcode with
				| And | Or -> 
					let bool_result = opcode_as_fun rei rfi in
					Bool (bool_result)
				| _ -> failwith "Cannot perform non-bool operations on bool values")
		| _, _ -> failwith ("Evaluation mismatch for operator: " ^ (string_of_opcode opcode))
and evaluate_operator_unary store opcode e = 	
	let result_e = evaluate_expression store e in
	match opcode with
	| Not -> (match result_e with
		| Bool (reb) -> Bool (not reb)
		| _ -> failwith "Invalid expression result for unary operator \"Not\"")



