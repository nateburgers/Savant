open Core.Std
(* Modelled after the Ollvm library by OcamlPro *)

module Support =
  struct
    let string_map_concat list ~f ?(sep="") =
      String.concat (List.map list ~f: f) ~sep: sep
    let add_flag option ~flag =
      if option then " " ^ flag else ""
    let add_flags flags =
      List.fold flags ~init: "" ~f: (fun acc (enable, name) ->
				     acc ^ add_flag enable ~flag: name)
  end

module Linkage =
  struct
    type t =
      | Private | Internal | Available_externally | Link_once | Weak
      | Common | Appending | Extern_weak | Linkonce_odr | Weak_odr | External
    let to_string = function
      | Private -> "private"
      | Internal -> "internal"
      | Available_externally -> "available_externally"
      | Link_once -> "linkonce"
      | Weak -> "weak"
      | Common -> "common"
      | Appending -> "appending"
      | Extern_weak -> "extern_weak"
      | Link_once_odr -> "linkonce_odr"
      | Weak_odr -> "weak_odr"
      | External -> "external"
  end

module DLL_storage =
  struct
    type t =
      | Import
      | Export
    let to_string = function
      | Import -> "dllimport"
      | Export -> "dllexport"
  end

module Visibility =
  struct
    type t =
      | Default
      | Hidden
      | Protected
    let to_string = function
      | Default -> "default"
      | Hidden -> "hidden"
      | Protected -> "protected"
  end

module Calling_convention =
  struct
    type t =
      | C
      | Fast
      | Cold
      | Numbered of int
    let to_string = function
      | C -> "ccc"
      | Fast -> "fastcc"
      | Cold - > "coldcc"
      | Numbered n -> "cc " ^ Int.to_string n
  end

module Type =
  struct
    type t =
      | Integer of int
      | Float16
      | Float32
      | Float64
      | Float128
      | Pointer of t
      | Void
      | Label
      | Metadata
      | Array of int * t
      | Function of t * t list
      | Struct of t list
      | Packed_struct of t list
      | Vector of int * t
      | Opaque
    let rec to_string = function
      | Integer n -> "i" ^ Int.to_string n
      | Float16 -> "half"
      | Float32 -> "float"
      | Float64 -> "double"
      | Float128 -> "fp128"
      | Pointer t -> to_string t ^ "*"
      | Void -> "void"
      | Label -> "label"
      | Metadata -> "metadata"
      | Array (length, t) ->
	 "[" ^ Int.to_string length ^ " x " ^ to_string t ^ "]"
      | Function (result, arguments) ->
	 let arguments_string =
	   String.concat (List.map arguments ~f: to_string) ~sep: ", " in
	 to_string result ^ "(" ^ arguments_string ^ ")"
      | Struct fields ->
	 "{" ^ String.concat (List.map fields ~f: to_string) ~sep: ", "  ^ "}"
      | Packed_struct fields ->
	 "<{" ^ String.concat (List.map fields ~f: to_string) ~sep: ", " ^ "}>"
      | Vector (length, t) ->
	 "<" ^ Int.to_string length ^ " x " ^ to_string t ^ ">"
      | Opaque -> "opaque"
  end

module Integer =
  struct
    module Comparison =
      struct
	type t =
	  | Eq | Ne | Ugt | Uge | Ult
	  | Ule | Sgt | Sge | Slt | Sle
	let to_string = function
	  | Eq -> "eq"
	  | Ne -> "ne"
	  | Ugt -> "ugt"
	  | Ult -> "ult"
	  | Ule -> "ule"
	  | Sgt -> "sgt"
	  | Sge -> "sge"
	  | Slt -> "slt"
	  | Sle -> "sle"
      end
    module Binary_operation =
      struct
	type sign_option = {
	    nuw: bool;
	    nsw: bool;
	  }
	let sign_option_to_string option =
	  let nuw = if option.nuw then " nuw" else "" in
	  let nsw = if option.nsw then " nsw" else "" in
	  nuw ^ nsw
	type exact_option = {
	    exact: bool;
	  }
	let exact_option_to_string option =
	  if option.exact then " exact" else ""
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
	let to_string = function
	  | Add option -> "add" ^ sign_option_to_string option
	  | Sub option -> "sub" ^ sign_option_to_string option
	  | Mul option -> "mul" ^ sign_option_to_string option
	  | Shl option -> "shl" ^ sign_option_to_string option
	  | UDiv exact -> "udiv" ^ exact_option_to_string exact
	  | SDiv exact -> "sdiv" ^ exact_option_to_string exact
	  | LShr exact -> "lshr" ^ exact_option_to_string exact
	  | Ashr exact -> "ashr" ^ exact_option_to_string exact
	  | URem -> "urem"
	  | SRem -> "srem"
	  | And -> "and"
	  | Or -> "or"
	  | Xor -> "xor"
      end
  end

module Float =
  struct
    module Comparison =
      struct
	type t =
	  | False | Oeq | Ogt | Oge | Olt
	  | Ole | One | Ord | Ueq
	  | Ugt | Uge | Ult | Ule | Une | Uno | True
	let to_string = function
	  | False -> "false"
	  | Oeq -> "oeq"
	  | Ogt -> "ogt"
	  | Oge -> "oge"
	  | Olt -> "olt"
	  | Ole -> "ole"
	  | One -> "one"
	  | Ord -> "ord"
	  | Ueq -> "ueq"
	  | Ugt -> "ugt"
	  | Uge -> "uge"
	  | Ult -> "ult"
	  | Ule -> "ule"
	  | Une -> "une"
	  | Uno -> "uno"
	  | True -> "true"
      end
    module Binary_operation =
      struct
	type flags = {
	    nnan: bool;
	    ninf: bool;
	    nsz: bool;
	    arcp: bool;
	    fast: bool;
	  }
	let flags_to_string flags =
	  Support.add_flags [ flags.nnan, "nnan"
			    ; flags.ninf, "ninf"
			    ; flags.nsz, "nsz"
			    ; flags.arcp, "arcp"
			    ; flags.fast, "fast"
			    ]
	type t =
	  | Add of flags
	  | Sub of flags
	  | Mul of flags
	  | Div of flags
	  | Rem of flags
	let to_string = function
	  | Add flags -> "fadd" ^ flags_to_string flags
	  | Sub flags -> "fsub" ^ flags_to_string flags
	  | Mul flags -> "fmul" ^ flags_to_string flags
	  | Div flags -> "fdiv" ^ flags_to_string flags
	  | Rem flags -> "frem" ^ flags_to_string flags
      end
  end

module Conversion =
  struct
    type t =
      | Trunc | Zext | Sext | Fptrunc | Fpext
      | Uitofp | Sitofp | Fptoui | Fptosi | Inttoptr
      | Ptrtoint | Bitcast
    let to_string = function
      | Trunc -> "trunc"
      | Zext -> "zext"
      | Sext -> "sext"
      | Fptrunc -> "fptrunc"
      | Fpext -> "fpext"
      | Uitofp -> "uitofp"
      | Sitofp -> "sitofp"
      | Fptoui -> "fptoui"
      | Fptosi -> "fptosi"
      | Inttoptr -> "inttoptr"
      | Ptrtoint -> "ptrtoint"
      | Bitcast -> "bitcast"
  end

module Identifier =
  struct
    type t =
      | Global of string
      | Local of string
     and typed = Type.t * t
    let to_string = function
      | Global i -> "@" ^ i
      | Local i -> "%" ^ i
    let typed_to_string (kind, identifier) =
      Type.to_string kind ^ " " ^ to_string identifier
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
    let rec to_string = function
      | Identifier i -> Identifier.to_string i
      | Integer i -> Int.to_string i
      | Float f -> Float.to_string f
      | Boolean b -> if b then "true" else "false"
      | Null -> "null"
      | Undefined -> "undef"
      | Struct elements ->
	 "{ " ^ Support.string_map_concat elements ~f: typed_to_string ~sep: ", " ^ " }"
      | Packed_struct elements ->
	 "<{ " ^ Support.string_map_concat elements ~f: typed_to_string ~sep: ", " ^ " }>"
      | Array elements ->
	 "[ " ^ Support.string_map_concat elements ~f: typed_to_string ~sep: ", " ^ " ]"
      | Vector elements ->
	 "< " ^ Support.string_map_concat elements ~f: typed_to_string ~sep: ", " ^ ">"
      | Zero_initializer -> "zeroinitializer"
    and typed_to_string (kind, value) =
      Type.to_string kind ^ " " ^ to_string value
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
      | Conversion of Conversion.t * Type.t * Value.t * Type.t
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
    let to_string = function
      | Integer_binary_operation (op, kind, lhs, rhs) ->
	 Integer.Binary_operation.to_string op ^ " " ^ Type.to_string kind
	 ^ " " Value.to_string lhs ^ ", " ^ Value.to_string rhs
      | Integer_comparison (cmp, kind, lhs, rhs) ->
	 "icmp " ^ Integer.Comparison.to_string cmp ^ " " ^ Type.to_string kind
	 ^ " " ^ Value.to_string lhs ^ ", " ^ Value.to_string rhs
      | Float_binary_operation (op, kind, lhs, rhs) ->
	 Float.Binary_operation.to_string op ^ " " ^ Type.to_string kind
	 ^ " " ^ Value.to_string lhs ^ ", " ^ Value.to_string rhs
      | Float_comparison (cmp, kind, lhs, rhs) ->
	 "fcmp " ^ Float.Comparison.to_string cmp ^ " " ^ Type.to_string kind
	 ^ " " ^ Value.to_string lhs ^ ", " ^ Value.to_string rhs
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
