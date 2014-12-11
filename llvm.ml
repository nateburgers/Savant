open Core.Std
(* Modelled after the Ollvm library by OcamlPro *)
module Linkage =
  struct
    type t =
      | Private | Internal | Available_externally | Link_once | Weak
      | Common | Appending | Extern_weak | Linkonce_odr | Weak_odr | External
  end

module DLL_storage =
  struct
    type t =
      | Import
      | Export
  end

module Visibility =
  struct
    type t =
      | Default
      | Hidden
      | Protected
  end

module Calling_convention =
  struct
    type t =
      | C
      | Fast
      | Cold
      | Numbered of int
  end

module Type =
  struct
    type t =
      | Integer of int
      | Float of int
      | Pointer of t
      | Void
      | Label
      | Metadata
      | Array of int * t
      | Function of t * t list
      | Struct of t list
      | Packed_struct of t list
      | Vector of int * t
  end

module Integer =
  struct
    module Comparison =
      struct
	type t =
	  | Eq | Ne | Ugt | Uge | Ult
	  | Ule | Sgt | Sge | Slt | Sle
      end
    module Binary_operation =
      struct
	type sign_option = {
	    nuw: bool;
	    nsw: bool;
	  }
	type exact_option = {
	    exact: bool;
	  }
	type t =
	  | Add of sign_option
	  | Sub of sign_option
	  | Mul of sign_option
	  | Shl of sign_option
	  | UDiv of exact_option
	  | SDiv of exact_option
	  | LShr of exact_option
	  | Ashr of exact_option
	  | URem | SRem | And | Or | Xor
      end
  end

module Float =
  struct
    module Comparison =
      struct
	type t =
	  | False | Oeq | Ogt | Oge | Olt
	  | Ole | One | Ord | Uno | Ueq
	  | Ugt | Uge | Ult | Ule | Une | True
      end
    module Binary_operation =
      struct
	type t =
	  | Add | Sub | Mul | Div | Rem
      end
  end

module Conversion =
  struct
    type t =
      | Trunc | Zext | Sext | Fptrunc | Fpext
      | Uitofp | Sitofp | Fptoui | Fptosi | Inttoptr
      | Ptrtoint | Bitcast
  end

module Identifier =
  struct
    type t =
      | Global of string
      | Local of string
     and typed = Type.t * t
  end

module Value =
  struct
    type t =
      | Identifier of Identifier.t
      | Integer of int
      | Float of float
      | Boolean of bool
      | Null
      | Undefined
      | Struct of typed list
      | Packed_struct of typed list
      | Array of typed list
      | Vector of typed list
      | Zero_initializer
     and typed = Type.t * t
  end

module Instruction =
  struct
    type volatile_option = {
	volatile: bool;
      }
    type t =
      | Integer_binary_operation of Integer.Binary_operation.t * Type.t * Value.t * Value.t
      | Integer_comparison of Integer.Comparison.t * Type.t * Value.t * Value.t
      | Float_binary_operation of Float.Binary_operation.t * Type.t * Value.t * Value.t
      | Float_comparison of Float.Comparison.t * Type.t * Value.t * Value.t
      | Conversion of Covnersion.t * Type.t * Value.t * Type.t
      | Get_element_pointer of Value.typed * Value.typed list
      | Extract_element of Value.typed * Value.typed
      | Insert_element of Value.typed * Value.typed * Value.typed
      | Shuffle_vector of Value.typed * Value.typed * Value.typed
      | Extract_value of Value.typed * int list
      | Insert_value of Value.typed * Value.typed * int list
      (* FIXME: declare what is going on with the optionals *)
      | Call of Identifier.typed * Value.typed list
      | Alloca of Type.t * Value.typed option * int option 
      | Load of volatile_option * Value.typed * int option
      | Phi of Type.t * (Value.t * Identifier.t) list
      | If of Value.typed * Value.typed * Value.typed
      | VAArg
      | Landing_pad
      | Store of volatile_option * Value.typed * Identifier.typed * int option
      | Fence
      | Atomic_compare_exchange
      | Atomic_read_modify_write
      | Invoke of Identifier.typed * Value.typed list * Identifier.typed * Identifier.typed
      | Return of Value.typed
      | Branch of Value.typed * Identifier.typed * Identifier.typed
      | Branch_1 of Identifier.typed
      | Switch of Value.typed * Identifier.typed * (Value.typed * Identifier.typed) list
      | Indirect_branch of Value.typed * Identifier.typed list
      | Resume of Value.typed
      | Unreachable
  end

module Global =
  struct
    type t = {
	identifier: Identifier.t;
	kind: Type.t;
	constant: bool;
	value: Value.t option;
	linkage: Linkage.t option;
      (* FIXME: add remaining optional parts *)
      }
  end

module Declaration =
  struct
    type t = {
	name: Identifier.t;
	kind: Type.t; 		(* restricted to functions *)
      (* FIXME: add attributes *)
      }
  end

module Block =
  struct
    type t = {
	label: string;
	instructions: Instruction.t list;
      }
  end

module Definition =
  struct
    type t = {
	prototype: Declaration.t;
	arguments: Identifier.t list;
	blocks: Block.t list;
	linkage: Linkage.t option;
      (* FIXME: add other parameters *)
      }
  end

module Top_level =
  struct
    type t =
      | Target of string
      | Data_layout of string
      | Declaration of Declaration.t
      | Definition of Definition.t
      | Type_declaration of Identifier.t * Type.t
      | Global of Global.t
      (* | Metadata of string * Metadata.t *)
      (* | Attribute_group of int * Function_attribute.t list *)
  end

module Module =
  struct
    type t = {
	name: string;
	target: Top_level.t;
	data_layout: Top_level.t;
	globals: (string * Global.t) list;
	declarations: (string * Declaration.t) list;
	definitions: (string * Definition.t) list;
      }
  end
