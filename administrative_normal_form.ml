open Core.Std
open Llvm

(* Administrative Normal Form is an intermediate representation that
 * can be easily converted into static single assignment form *)

module Support =
  struct
    module Infix =
      struct
	let (@>) f x = f x
      end
  end

module Type =
  struct
    open Support.Infix
    type t =
      | Label
      | Natural of int
      | Integer of int
      | Real of int
      | Character
      | Pointer of t
      | Vector of t
      | Tuple of t list
      | Function of t * t
    let unary_function argument ~result =
      Function (argument, result)
    let binary_function argument_1 argument_2 ~result =
      Function (Tuple [argument_1; argument_2], result)
    let binary_operator operand =
      binary_function operand operand ~result: operand
    let rec to_string = function
      | Label -> "Label"
      | Natural n -> "Natural" ^ Int.to_string n
      | Integer n -> "Integer" ^ Int.to_string n
      | Real n -> "Real" ^ Int.to_string n
      | Character -> "Character"
      | Pointer t -> "@" ^ to_string t
      | Vector element -> "<" ^ to_string element ^ ">"
      | Tuple elements -> "(" ^ String.concat (List.map elements ~f: to_string) ~sep:", " ^ ")"
      | Function (Function _ as f, result) -> "(" ^ to_string f ^ ")" ^ " -> " ^ to_string result
      | Function (argument, result) -> to_string argument ^ " -> " ^ to_string result
  end

module Primitive_operation =
  struct
    type t =
      | Allocate of Type.t
      | Natural_add of int
      | Natural_subtract of int
    let type_of = function
      | Allocate kind -> Type.Function (Tuple [], kind)
      | Natural_add width -> Type.binary_operator (Type.Natural width)
      | Natural_subtract width -> Type.binary_operator (Type.Natural width)
    let to_llvm = function
      | Natural_add _ -> Instruction.add ~nsw: false ~nuw: false
      | Natural_subtract _ -> Instruction.sub ~nsw: false ~nuw: false
    let to_string = function
      | Allocate _ -> "allocate"
      | Natural_add _ -> "natural_add"
      | Natural_subtract _ -> "natural_subtract"
  end

module Terminal =
  struct
    open Support.Infix
    exception Empty_vector
    type t =
      | Variable of string
      | Primitive of Primitive_operation.t
      | Natural of int
      | Integer of int
      | Real of float
      | Character of char
      | Vector of t list
      | Tuple of t list
    let free_variables = function
      | Variable v -> String.Set.singleton v
      | _ -> String.Set.empty
    let rec type_of_exn = function
      | Variable v -> Type.Label
      | Primitive p -> Primitive_operation.type_of p
      | Natural _ -> Type.Natural 64
      | Integer _ -> Type.Integer 64
      | Real _ -> Type.Real 64
      | Character _ -> Type.Character
      | Vector [] -> raise Empty_vector
      | Vector (v::vs) -> Type.Vector (type_of_exn v)
      | Tuple elements -> Type.Tuple (List.map elements ~f: type_of_exn)
    let rec to_string = function
      | Variable v -> v
      | Primitive p -> Primitive_operation.to_string p
      | Natural n -> Int.to_string n
      | Integer n -> Int.to_string n
      | Real f -> Float.to_string f
      | Character c -> Char.to_string c
      | Vector elements -> "<" ^ String.concat (List.map elements ~f: to_string) ~sep:", " ^ ">"
      | Tuple elements -> "(" ^ String.concat (List.map elements ~f: to_string) ~sep:", " ^ ")"
    let rec to_string_with_types terminal =
      to_string terminal ^ ": " ^ (Type.to_string @> type_of_exn terminal)
  end

module Attempt =
  struct
    type ('a, 'b) t =
      | Success of 'a
      | Failure of 'b
    let of_option option ~error =
      match option with
      | Some value -> Success value
      | None -> Failure error
    let to_exn attempt ~error_to_exn =
      match attempt with
      | Success value -> value
      | Failure error -> raise (error_to_exn error)
    let bind attempt ~f =
      match attempt with
      | Success value -> f value
      | Failure error -> Failure error
    module Infix =
      struct
	let (>>=) attempt f = bind attempt ~f:f
      end
  end

module Static_single_assignment =
  struct
    open Support.Infix
    open Attempt.Infix
    module Instruction =
      struct
	(* Instruction ::= Let | If | Return
	 * Let Alias ::= "let" , variable , "=" , value , "in"
	 * Let Call ::= "let" , variable , "=" , function name , argument , "in"
	 * If ::= "if" , condition , "then" , label_1 , "else" , label_2
	 * Return ::= "return" , value
	 *)
	type t =
	  | Let_alias of string * Terminal.t
	  | Let_call of string * string * Terminal.t
	  | Let_phi of string * string * string
	  | If of string * string * string
	  | Return of Terminal.t
	let to_string = function
	  | Let_alias (name, value) ->
	     "let " ^ name ^ " = " ^ Terminal.to_string_with_types value
	  | Let_call (name, function_name, argument) ->
	     "let " ^ name ^ " = " ^ function_name ^ " " ^ Terminal.to_string_with_types argument
	  | Let_phi (name, left_label, right_label) ->
	     "let " ^ name ^ " = phi (" ^ left_label ^ ", " ^ right_label ^ ")"
	  | If (variable, then_label, else_label) ->
	     "if " ^ variable ^ " then " ^ then_label ^ " else " ^ else_label
	  | Return terminal ->
	     "return " ^ Terminal.to_string_with_types terminal
      end
    module Basic_block =
      struct
	type t = {
	    label: string;
	    body: Instruction.t list;
	  }
	let to_string basic_block =
	  let indent_instruction i = "  " ^ Instruction.to_string i in
	  let block_body = String.concat (List.map basic_block.body ~f: indent_instruction) ~sep: "\n" in
	  basic_block.label ^ ":\n" ^ block_body
      end
    module Function =
      struct
	type t = {
	    name: string;
	    arguments: (string * Type.t) list;
	    result: Type.t;
	    blocks: Basic_block.t list;
	  }
	let to_string func =
	  let argument_string (name, kind) = name ^ ": " ^ Type.to_string kind in
	  let argument_list = String.concat (List.map func.arguments ~f: argument_string) ~sep: ", " in
	  "function " ^ func.name ^ " (" ^ argument_list ^ "): " ^ Type.to_string func.result ^ "\n"
	  ^ String.concat (List.map func.blocks ~f: Basic_block.to_string) ~sep: "\n" ^ "\n"
	  ^ "end"
      end
    type t = Function.t list
    let to_string program =
      String.concat (List.map program ~f: Function.to_string) ~sep: "\n\n"
    let debug_function =
      { Function.name = "Main";
	Function.arguments = ["something", Type.Integer 64];
	Function.result = Type.Label;
	Function.blocks =
	  [ { Basic_block.label = "start";
	      Basic_block.body =
		[ Instruction.Let_alias ("x", Terminal.Natural 455)
		; Instruction.Return (Terminal.Variable "x")
		]
	    }
	  ]
      }
    let debug = print_string @> to_string [debug_function; debug_function]
  end
module SSA = Static_single_assignment

module Expression =
  struct
    open Support.Infix
    open Attempt.Infix
    type error =
      | Unbound_identifier of string
    exception Error of error
    type t =
      | Value of Terminal.t
      (* Call : function name, argument *)
      | Call of string * Terminal.t
      (* Let Copy : variable name, value, continuation *)
      | Let_copy of string * Terminal.t * t
      (* Let Call : variable name, function name, argument, continuation *)
      | Let_call of string * string * Terminal.t * t
      (* Let Recursive : function name, argument name, argument type, body, continuation *)
      | Let_recursive of string * string * Type.t * t * t
      (* Branch : condition, positive branch, negative branch *)
      | Branch of Terminal.t * t * t
    ;;
    let of_primitive_operation operation =
      Value (Terminal.Primitive operation)
    ;;
    let built_in_functions =
      String.Map.of_alist_exn [ "add", of_primitive_operation (Primitive_operation.Natural_add 64)
			      ; "subtract", of_primitive_operation (Primitive_operation.Natural_subtract 64)
			      ]
    ;;
    let rec to_ssa expression ~name_bindings =
      match expression with
      | Let_recursive (function_name, argument_name, argument_type, body, continuation) ->
	 ()
    ;;
    let rec to_llvm expression ~name_bindings =
      match expression with
      | Value (Terminal.Variable name) ->
	 Attempt.of_option (String.Map.find name_bindings name) ~error: (Unbound_identifier name)
	 >>= (function value -> Attempt.Success value)
      | _ -> Attempt.Failure (Unbound_identifier "oh no")
    ;;
    let to_llvm_exn expression ?(name_bindings = String.Map.empty) =
      Attempt.to_exn (to_llvm expression ~name_bindings: name_bindings) ~error_to_exn: (fun x -> Error x) 
    ;;
    let rec free_variables = function
      | Value v -> Terminal.free_variables v
      | Call (function_name, argument) -> String.Set.add (Terminal.free_variables argument) function_name
      | Let_copy (value_name, value, continuation) ->
	 let free_variables_in_value = Terminal.free_variables value in
	 let free_variables_in_continuation = String.Set.remove (free_variables continuation) value_name in
	 String.Set.union free_variables_in_value free_variables_in_continuation
      | Let_call (value_name, function_name, argument, continuation) ->
	 let free_variables_in_application = String.Set.add (Terminal.free_variables argument) function_name in
	 let free_variables_in_continuation = String.Set.remove (free_variables continuation) value_name in
	 String.Set.union free_variables_in_application free_variables_in_continuation
      | Let_recursive (function_name, argument_name, argument_type, body, continuation) ->
	 let free_variables_in_body = String.Set.remove (free_variables body) argument_name in
	 let free_variables_in_continuation = String.Set.remove (free_variables continuation) function_name in
	 String.Set.union free_variables_in_body free_variables_in_continuation
      | Branch (condition, then_case, else_case) ->
	 let free_variables_in_body = String.Set.union (free_variables then_case) (free_variables else_case) in
	 String.Set.union free_variables_in_body (Terminal.free_variables condition)
  end
