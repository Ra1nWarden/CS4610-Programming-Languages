(*Zihao Wang*)
(*zw2rf@virginia.edu*)
(*April 14, 2014*)

(*These types are used for keys and values in Environment map and Store*)
type variable = Variable of string
and location = Location of int
and field = Field of string * location
and value = Bool of bool
	    | Int of int32
	    | String of string * int (*content, length*)
	    | Object of string * field list * location (*type, fields*)
			| Nulldefault of bool

(*Other cool types/expressions*)
and identifier = Identifier of string * string (*id_name, line_number*)
and let_binding = Let_binding of identifier * identifier * expr option (*variable, type, init*)
and case_element = Case_element of identifier * identifier * expr (*variable, type, body*)
and expr = Assign of identifier * identifier * expr * string (*exp_id, var, rhs*)
	   | Dynamic_dispatch of identifier * expr * identifier * expr list option * string (*exp_id, e, method, args*)
	   | Static_dispatch of identifier * expr * identifier * identifier * expr list option * string (*exp_id, e, type, method, args*)
	   | Self_dispatch of identifier * identifier * expr list option * string (*exp_id, method, args*)
	   | If of identifier * expr * expr * expr * string (*exp_id, pred, then, else*)
	   | While of identifier * expr * expr * string (*exp_id, pred, body*)
	   | Block of identifier * expr list option * string (*exp_id, body*)
	   | New of identifier * identifier * string (*exp_id, class*)
	   | Isvoid of identifier * expr * string (*exp_id, e*)
	   | Plus of identifier * expr * expr * string (*exp_id, x, y*)
	   | Minus of identifier * expr * expr * string (*exp_id, x, y*)
	   | Times of identifier * expr * expr * string (*exp_id, x, y*)
	   | Divide of identifier * expr * expr * string (*exp_id, x, y*)
	   | Lt of identifier * expr * expr * string (*exp_id, x, y*)
	   | Le of identifier * expr * expr * string (*exp_id, x, y*)
	   | Eq of identifier * expr * expr * string (*exp_id, x, y*)
	   | Not of identifier * expr * string (*exp_id, x*)
	   | Negate of identifier * expr * string (*exp_id, x*)
	   | Integer_exp of identifier * int32 * string (*exp_id, cons*)
	   | String_exp of identifier * string * string (*exp_id, cons*)
	   | Identifier_exp of identifier * identifier * string (*exp_id, id*)
	   | Bool_exp of identifier * bool * string (*exp_id, cons*)
	   | Let_exp of identifier * let_binding list * expr * string (*exp_id, bindings, body*)
	   | Case_exp of identifier * expr * case_element list * string (*exp_id, case_body, elements*)
		 | Internal_exp of identifier * string * string (*exp_id, class.method, return_type*)

(*cool structures*)
and formal = Formal of string (*just the name of the formal*)
and feature = Attribute of string * string * expr option (*attribute name, type name, init exp*)
	      | Method of string * string * formal list * expr option (*method name, ultimate parent for def, formals, body*)
and cclass = Cclass of identifier * identifier option * feature list option
and program = Program of cclass list
and classmethod = ClassMethod of string * string

module LocationMap = Map.Make (struct type t = location let compare = compare end)
module EnvironmentMap = Map.Make (struct type t = variable let compare = compare end)
module ClassMap = Map.Make (String)
module ImplementationMap = Map.Make (struct type t = classmethod let compare = compare end)
module ParentMap = Map.Make (String)

(*file read function*)
let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do
      lines := input_line chan :: !lines
    done;
    !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines

(*helper functions*)
let rec drop count lst = match count with
    1 -> List.tl lst
  | _ -> drop (count-1) (List.tl lst)

(*newloc update function*)
let nextloc = ref 1
let newloc origloc = (origloc := !origloc + 1);
										!origloc

(*track stack overflow*)
let stacks = ref 0
let newstack origstack = (origstack := !origstack + 1);
												 !origstack
let finishstack origstack = (origstack := !origstack - 1);
														!origstack

(*processing inputs*)
let rec processExpression exp_str =
  let processIdentifier str_lst =
    Identifier (List.nth str_lst 0, List.nth str_lst 1)
  in
  let processLetBindings str_lst =
    let count = int_of_string (List.hd str_lst) in
    let processLetBinding str_binding =
      match str_binding with
	  		"let_binding_no_init" :: lst -> let this_var = processIdentifier lst in
					let this_type = processIdentifier (drop 2 lst) in
					(Let_binding (this_var, this_type, None), drop 4 lst)
			| "let_binding_init" :: lst -> let this_var = processIdentifier lst in
				   let this_type = processIdentifier (drop 2 lst) in
				   let this_init = processExpression (drop 4 lst) in
				   (Let_binding (this_var, this_type, Some(fst this_init)), snd this_init)
    in
    let rec getBindings x y = match y with
				1 -> let this_binding = processLetBinding x in
	     			([fst this_binding], snd this_binding)
      | _ -> let this_binding = processLetBinding x in
	     			let rest_binding = getBindings (snd this_binding) (y-1) in
	           ([fst this_binding] @ (fst rest_binding), snd rest_binding)
    in
    getBindings (List.tl str_lst) count
  in
  let processCaseElements str_lst =
    let count = int_of_string (List.hd str_lst) in
    let processCaseElement str_case =
      let this_variable = processIdentifier str_case in
      let this_type = processIdentifier (drop 2 str_case) in
      let exp_remain = processExpression (drop 4 str_case) in
      (Case_element (this_variable, this_type, fst exp_remain), snd exp_remain)
    in
    let rec getCaseElements x y =
      match y with 1 -> let this_case_element = processCaseElement x in
								([fst this_case_element], snd this_case_element)
  	         		| _ -> let this_case_element = processCaseElement x in
								let rest_case_element = getCaseElements (snd this_case_element) (y-1) in
  							([fst this_case_element] @ (fst rest_case_element), snd rest_case_element)
    in
    getCaseElements (List.tl str_lst) count
  in
  let processExpressionList lst = let count = int_of_string (List.hd lst) in
				  match count with 0 -> (None, List.tl lst)
				                 | _ -> let rec findExpressionsAndRemain x y = match y with 1 -> let first_exp = processExpression x in
														 																														([fst first_exp], snd first_exp)
						                                                          						| _ -> let first_exp = processExpression x in
														 																														let remain_exp = findExpressionsAndRemain (snd first_exp) (y-1) in
														 																													  ([fst first_exp] @ (fst remain_exp), snd remain_exp)
																in
																let result = findExpressionsAndRemain (List.tl lst) count in
																(Some(fst result), snd result)
  in
  match exp_str with
    lineno :: return_type :: "assign" :: lst -> let rhs_exp = processExpression (drop 2 lst) in
				 																			 let var = processIdentifier lst in
				 											  				 	    let exp_id = Identifier ("assign", lineno) in
				 											         		    (Assign (exp_id, var, fst rhs_exp, return_type), snd rhs_exp)
  | lineno :: return_type :: "dynamic_dispatch" :: lst -> let exp_id = Identifier ("dynamic_dispatch", lineno) in
					   																						 let this_e = processExpression lst in
					   													    						   let this_method = processIdentifier (snd this_e) in
					   													    						   let this_exp_list_remain = processExpressionList (drop 2 (snd this_e)) in
					   													    							 (Dynamic_dispatch (exp_id, fst this_e, this_method, fst this_exp_list_remain, return_type), snd this_exp_list_remain)
  | lineno :: return_type :: "static_dispatch" :: lst -> let exp_id = Identifier ("static_dispatch", lineno) in
                                          							 let this_e = processExpression lst in
					  																					   let this_type = processIdentifier (snd this_e) in
					  																					   let this_method = processIdentifier (drop 2 (snd this_e)) in
					  																					   let this_exp_list_remain = processExpressionList (drop 4 (snd this_e)) in
					  																					   (Static_dispatch (exp_id, fst this_e, this_type, this_method, fst this_exp_list_remain, return_type), snd this_exp_list_remain)
  | lineno :: return_type :: "self_dispatch" :: lst -> let exp_id = Identifier ("self_dispatch", lineno) in
																											 let this_method = processIdentifier lst in
																											 let this_exp_list_remain = processExpressionList (drop 2 lst) in
																											 (Self_dispatch (exp_id, this_method, fst this_exp_list_remain, return_type), snd this_exp_list_remain)
  | lineno :: return_type :: "if" :: lst -> let exp_id = Identifier ("if", lineno) in
			     																 let this_pred = processExpression lst in
			     						      							 let this_then = processExpression (snd this_pred) in
			     						      							 let this_else = processExpression (snd this_then) in
			     						      							 (If (exp_id, fst this_pred, fst this_then, fst this_else, return_type), snd this_else)
  | lineno :: return_type :: "while" :: lst -> let exp_id = Identifier ("while", lineno) in
																							 let this_while = processExpression lst in
																							 let this_body = processExpression (snd this_while) in
																							 (While (exp_id, fst this_while, fst this_body, return_type), snd this_body)
  | lineno :: return_type :: "block" :: lst -> let exp_id = Identifier ("block", lineno) in
																							 let this_exp_list_remain = processExpressionList lst in
																							 (Block (exp_id, fst this_exp_list_remain, return_type), snd this_exp_list_remain)
  | lineno :: return_type :: "new" :: lst -> let exp_id = Identifier ("new", lineno) in
			      																 let this_class_id = processIdentifier lst in
			      													       (New (exp_id, this_class_id, return_type), drop 2 lst)
  | lineno :: return_type :: "isvoid" :: lst -> let exp_id = Identifier ("isvoid", lineno) in
				 																			 let this_is_void = processExpression lst in
				 											  							 (Isvoid (exp_id, fst this_is_void, return_type), snd this_is_void)
  | lineno :: return_type :: "plus" :: lst -> let exp_id = Identifier ("plus", lineno) in
			       															   let this_x = processExpression lst in
			       					        							 let this_y = processExpression (snd this_x) in
			       					        							 (Plus (exp_id, fst this_x, fst this_y, return_type), snd this_y)
  | lineno :: return_type :: "minus" :: lst -> let exp_id = Identifier ("minus", lineno) in
																							 let this_x = processExpression lst in
																							 let this_y = processExpression (snd this_x) in
																							 (Minus (exp_id, fst this_x, fst this_y, return_type), snd this_y)
  | lineno :: return_type :: "times" :: lst -> let exp_id = Identifier ("times", lineno) in
																							 let this_x = processExpression lst in
																							 let this_y = processExpression (snd this_x) in
																							 (Times (exp_id, fst this_x, fst this_y, return_type), snd this_y)
  | lineno :: return_type :: "divide" :: lst -> let exp_id = Identifier ("divide", lineno) in
				 																			 let this_x = processExpression lst in
				 											  							 let this_y = processExpression (snd this_x) in
				 											  							 (Divide (exp_id, fst this_x, fst this_y, return_type), snd this_y)
  | lineno :: return_type :: "lt" :: lst -> let exp_id = Identifier ("lt", lineno) in
			     																 let this_x = processExpression lst in
			     						      							 let this_y = processExpression (snd this_x) in
			     						      							 (Lt (exp_id, fst this_x, fst this_y, return_type), snd this_y)
  | lineno :: return_type :: "le" :: lst -> let exp_id = Identifier ("le", lineno) in
			     																 let this_x = processExpression lst in
			     						      							 let this_y = processExpression (snd this_x) in
			     						      							 (Le (exp_id, fst this_x, fst this_y, return_type), snd this_y)
  | lineno :: return_type :: "eq" :: lst -> let exp_id = Identifier ("eq", lineno) in
			     																 let this_x = processExpression lst in
			     						      							 let this_y = processExpression (snd this_x) in
			     						      							 (Eq (exp_id, fst this_x, fst this_y, return_type), snd this_y)
  | lineno :: return_type :: "not" :: lst -> let exp_id = Identifier ("not", lineno) in
			      																 let this_x = processExpression lst in
			      													 			(Not (exp_id, fst this_x, return_type), snd this_x)
  | lineno :: return_type :: "negate" :: lst -> let exp_id = Identifier ("negate", lineno) in
				 																			 let this_x = processExpression lst in
				 											  							 (Negate (exp_id, fst this_x, return_type), snd this_x)
  | lineno :: return_type :: "integer" :: lst -> let exp_id = Identifier ("integer", lineno) in
				  																			 let constant = int_of_string (List.hd lst) in
				  																		   (Integer_exp (exp_id, Int32.of_int constant, return_type), List.tl lst)
  | lineno :: return_type :: "string" :: lst -> let exp_id = Identifier ("string", lineno) in
				 																			 let constant = List.hd lst in
				 											  							 (String_exp (exp_id, constant, return_type), List.tl lst)
  | lineno :: return_type :: "identifier" :: lst -> let exp_id = Identifier ("identifier", lineno) in
				     																			 let this_id = processIdentifier lst in
				     									      							 (Identifier_exp (exp_id, this_id, return_type), drop 2 lst)
  | lineno :: return_type :: "true" :: lst -> let exp_id = Identifier ("true", lineno) in
			       																 (Bool_exp (exp_id, true, return_type), lst)
  | lineno :: return_type :: "false" :: lst -> let exp_id = Identifier ("false", lineno) in
																							 (Bool_exp (exp_id, false, return_type), lst)
  | lineno :: return_type :: "let" :: lst -> let exp_id = Identifier ("let", lineno) in
			      																 let bindings = processLetBindings lst in
			      													 			let let_body = processExpression (snd bindings) in
			      													 			(Let_exp (exp_id, fst bindings, fst let_body, return_type), snd let_body)
  | lineno :: return_type :: "case" :: lst -> let exp_id = Identifier ("case", lineno) in
  			       															 let this_case =processExpression lst in
  			       			          							 let this_case_elements = processCaseElements (snd this_case) in
  			       			          							 (Case_exp (exp_id, fst this_case, fst this_case_elements, return_type), snd this_case_elements)
	| lineno :: return_type :: "internal" :: lst -> let exp_id = Identifier ("internal", lineno) in
																									let classmethodstr = List.hd lst in
																									(Internal_exp (exp_id, classmethodstr, return_type), List.tl lst)

(*Find least common ancestor which is necessary for case exp*)
let findLeastCommonAncestor compare_str class_list parentmap = let rec getInheritanceTree current_class formed_list parentmap = match current_class with
																																																																	"Object" -> formed_list @ [current_class]
																																																																| _ -> let parent_class = ParentMap.find current_class parentmap in
																																																																			 getInheritanceTree parent_class (formed_list @ [current_class]) parentmap
																															 in
																															 let inheritanceTree = getInheritanceTree compare_str [] parentmap in
																															 let result_opt = List.fold_left (fun acc elem -> match acc with
																																 																								 Some (e) -> Some (e)
																																															 								 | None -> (match (List.mem elem class_list) with
																																															   					 								 true -> Some (elem)
																																																													 | false -> None)) None inheritanceTree
																														 	in
																															 match result_opt with
																															   Some (str) -> str
																															 | None -> ""

(*This function is used to get line number for reporting*)
let getExpressionLineNumber exp = match exp with
		Assign (Identifier (_, num), _, _, _) -> num
	| Dynamic_dispatch (Identifier (_, num), _, _, _, _) -> num
	| Static_dispatch (Identifier (_, num), _, _, _, _, _) -> num
	| Self_dispatch (Identifier (_, num), _, _, _) -> num
	| If (Identifier (_, num), _, _, _, _) -> num
	| While (Identifier (_, num), _, _, _) -> num
	| Block (Identifier (_, num), _, _) -> num
	| New (Identifier (_, num), _, _) -> num
	| Isvoid (Identifier (_, num), _, _) -> num
	| Plus (Identifier (_, num), _, _, _) -> num
	| Minus (Identifier (_, num), _, _, _) -> num
	| Times (Identifier (_, num), _, _, _) -> num
	| Divide (Identifier (_, num), _, _, _) -> num
	| Lt (Identifier (_, num), _, _, _) -> num
	| Le (Identifier (_, num), _, _, _) -> num
	| Eq (Identifier (_, num), _, _, _) -> num
	| Not (Identifier (_, num), _, _) -> num
	| Negate (Identifier (_, num), _, _) -> num
	| Integer_exp (Identifier (_, num), _, _) -> num
	| String_exp (Identifier (_, num), _, _) -> num
	| Identifier_exp (Identifier (_, num), _, _) -> num
	| Bool_exp (Identifier (_, num), _, _) -> num
	| Let_exp (Identifier (_, num), _, _, _) -> num
	| Case_exp (Identifier (_, num), _, _, _) -> num
	| Internal_exp (Identifier (_, num), _, _) -> num

(*Evaluate expressions according to the operational semantics rules*)
let rec evaluateExpression so environment store classmap implmap parentmap exp = match exp with
		Assign (_, variable, rhs_exp, _) -> let variable_string = match variable with
																																Identifier (_, name) -> name
																				in
																				let (value, store2) = evaluateExpression so environment store classmap implmap parentmap rhs_exp in
																				let var_location = EnvironmentMap.find (Variable (variable_string)) environment in
																				let store3 = LocationMap.add var_location value store2 in
																				(value, store3)
	| Identifier_exp (_, variable_id, _) -> let variable_string = match variable_id with
																																  Identifier (_, name) -> name
																			    in
																			    (match variable_string with
																			       "self" -> (so, store)
																			     | _ -> let var_location = EnvironmentMap.find (Variable (variable_string)) environment in
																			 			     let return_value = LocationMap.find var_location store in
																					 (return_value, store))
	| Bool_exp (_, bool_val, _) -> let value = match bool_val with
	 																						true -> Bool (true)
																						 | false -> Bool (false)
																 in
																 (value, store)
	| Integer_exp (_, int_val, _) -> let value = Int (int_val) in
																	 (value, store)
	| String_exp (_, str_val, _) -> let value = String (str_val, String.length str_val) in
																	(value, store)
	| New (_, class_id, _) -> let newstackcount = newstack stacks in
														let status_code = match newstackcount < 1000 with
																								false -> let line_number = getExpressionLineNumber exp in
																												 let status = Printf.printf("ERROR: %s: Exception: stack overflow\n") line_number in
																												 let exit_status = Pervasives.exit(0) in
																												 ""
																							| true -> ""
														in
														let variable_string = match class_id with
																										Identifier (_, name) -> name
														in
														let valueclass = match variable_string with
																							 "SELF_TYPE" -> (match so with
																							  								 Object (clsname, _, _) -> clsname)
													  								 | _ -> variable_string
														in
														let attributes_list = ClassMap.find valueclass classmap
														in
														let newlocations = List.fold_left (fun acc elem -> let oneloc = newloc nextloc in
																																							 acc @ [Location (oneloc)]) [] attributes_list in
													  let newemptyfield = List.fold_left2 (fun acc elem1 elem2 -> let attribute_name = match elem1 with
																																																							 (Attribute (attrname, _, _)) -> attrname
																																												in
																																												acc @ [Field (attribute_name, elem2)]) [] attributes_list newlocations in
														let newemptyvalue = Object (valueclass, newemptyfield, Location (newloc nextloc)) in
														let store2 = List.fold_left2 (fun acc elem1 elem2 -> let attribute_type = match elem1 with
																																																				(Attribute (_, typename, _)) -> typename
																																								 in
																																								 match attribute_type with
																																								   "Int" -> LocationMap.add elem2 (Int (Int32.of_int 0)) acc
																																								 | "String" -> LocationMap.add elem2 (String ("", 0)) acc
																																							   | "Bool" -> LocationMap.add elem2 (Bool (false)) acc
																																								 | _ -> LocationMap.add elem2 (Nulldefault (true)) acc) store attributes_list newlocations in
														let subenvironment = List.fold_left2 (fun acc elem1 elem2 -> let attribute_name = match elem1 with
																																																							(Attribute (attrname, _, _)) -> attrname
																																												 in
																																												 EnvironmentMap.add (Variable (attribute_name)) elem2 acc) EnvironmentMap.empty attributes_list newlocations in
													  let store3 = List.fold_left2 (fun acc elem1 elem2 -> match elem1 with
															 																									  (Attribute (_, _, None)) -> acc
																																					 			| (Attribute (_, _, Some(e))) -> let (elemval, storenew) = evaluateExpression newemptyvalue subenvironment acc classmap implmap parentmap e in
																																									 															 LocationMap.add elem2 elemval storenew)
																				 store2 attributes_list newlocations
														in
														let updatestackcount = finishstack stacks in
													  (newemptyvalue, store3)
	| Dynamic_dispatch (_, e0, methodId, exps_option, _) -> let newstackcount = newstack stacks in
																													let status_code = match newstackcount < 1000 with
																																							false -> let line_number = getExpressionLineNumber exp in
																																											 let status = Printf.printf("ERROR: %s: Exception: stack overflow\n") line_number in
																																											 let exit_status = Pervasives.exit(0) in
																																											 ""
																																						| true -> ""
																													in
																													let (vs, storen) = match exps_option with
																																				 			None -> ([], store)
																																			 			| Some(exps) -> List.fold_left (fun acc elem -> let (valelem, storelem) = evaluateExpression so environment (snd acc) classmap implmap parentmap elem in
																																			 																											let oldvals = fst acc in
																																																														 (oldvals @ [valelem], storelem)) ([], store) exps
																													in
																													let (v0, sn2) = evaluateExpression so environment storen classmap implmap parentmap e0 in
																													let target_name = match v0 with
																																						  Bool (_) -> "Bool"
																																						| Int (_) -> "Int"
																																						| String (_, _) -> "String"
																																						| Object (x, _, _) -> x
																																					  | Nulldefault (_) -> let line_number = getExpressionLineNumber e0 in
																																																 let status = Printf.printf("ERROR: %s: Exception: dynamic dispatch on void\n") line_number in
																																																 let exit_status = Pervasives.exit(0) in
																																																 ""
																													in
																													let method_name = match methodId with
																																										(Identifier (_, x)) -> x
																													in
																													let method_lookedup = ImplementationMap.find (ClassMethod (target_name, method_name)) implmap in
																													let arguments_list = match method_lookedup with
																																								 (Method (_, _, argslist, _)) -> argslist in
																													let newlocations = List.fold_left (fun acc elem -> let oneloc = newloc nextloc in
																																																						acc @ [Location (oneloc)]) [] arguments_list in
																													let sn3 = List.fold_left2 (fun acc elem1 elem2 -> LocationMap.add elem1 elem2 acc) sn2 newlocations vs in
																													let subenvironment = let fieldlist = List.fold_left2 (fun acc elem1 elem2 -> let variable_field = match elem1 with
																																																																															(Formal (arg_str)) -> arg_str
																																																																				in
																																																																				acc @ [Field (variable_field, elem2)]) [] arguments_list newlocations
																																								in
																																								let attributes_list = match v0 with
																																																				Object (_, fieldslist, _) -> fieldslist
																																																			| _ -> []
																																								in
																																								let empty_map = EnvironmentMap.empty in
																																								let half_map = List.fold_left (fun acc elem -> match elem with
																																																									(Field (field_var, field_loc)) -> EnvironmentMap.add (Variable (field_var)) field_loc acc) empty_map attributes_list
																																								in
																																								List.fold_left (fun acc elem -> let elemtuple = match elem with
																																																																								Field (attrname, loc) -> (Variable (attrname), loc)
																																																															in
																																																															EnvironmentMap.add (fst elemtuple) (snd elemtuple) acc) half_map fieldlist
																													in
																													let method_body = match method_lookedup with
																																										(Method (_, _, _, Some(exp_body))) -> exp_body in
																													let retval = evaluateExpression v0 subenvironment sn3 classmap implmap parentmap method_body in
																													let updatestackcount = finishstack stacks in
																													retval
		| Self_dispatch (_, methodId, exps_option, _) -> let newstackcount = newstack stacks in
																										 let status_code = match newstackcount < 1000 with
																						 														false -> let line_number = getExpressionLineNumber exp in
																																									let status = Printf.printf("ERROR: %s: Exception: stack overflow\n") line_number in
																																									let exit_status = Pervasives.exit(0) in
																																									""
									 																										| true -> ""
																										 in
																										 let (vs, storen) = match exps_option with
																																								None -> ([], store)
																																							| Some(exps) -> List.fold_left (fun acc elem -> let (valelem, storelem) = evaluateExpression so environment (snd acc) classmap implmap parentmap elem in
																																																															let oldvals = fst acc in
																																																															(oldvals @ [valelem], storelem)) ([], store) exps
																										 in
																										 let target_name = match so with
																										 										Object (x, _, _) -> x
																										 in
																										 let method_name = match methodId with
																																								(Identifier (_, x)) -> x
																									   in
																										 let method_lookedup = ImplementationMap.find (ClassMethod (target_name, method_name)) implmap in
																										 let arguments_list = match method_lookedup with
																																						(Method (_, _, argslist, _)) -> argslist in
																										 let newlocations = List.fold_left (fun acc elem -> let oneloc = newloc nextloc in
																																																				acc @ [Location (oneloc)]) [] arguments_list in
																										 let sn1 = List.fold_left2 (fun acc elem1 elem2 -> LocationMap.add elem1 elem2 acc) storen newlocations vs in
																										let subenvironment = let fieldlist = List.fold_left2 (fun acc elem1 elem2 -> let variable_field = match elem1 with
																																																																												(Formal (arg_str)) -> arg_str
																																																																	in
																																																																	acc @ [Field (variable_field, elem2)]) [] arguments_list newlocations
																																				in
																																				let attributes_list = match so with
																																																Object (_, fieldslist, _) -> fieldslist
																																															| _ -> []
																																				in
																																				let empty_map = EnvironmentMap.empty in
																																				let half_map = List.fold_left (fun acc elem -> match elem with
																																																					(Field (field_var, field_loc)) -> EnvironmentMap.add (Variable (field_var)) field_loc acc) empty_map attributes_list
																																				in
																																				List.fold_left (fun acc elem -> let elemtuple = match elem with
																																																														Field (attrname, loc) -> (Variable (attrname), loc)
																																																				in
																																																				EnvironmentMap.add (fst elemtuple) (snd elemtuple) acc) half_map fieldlist
																										in
																										let method_body = match method_lookedup with
																																								(Method (_, _, _, Some(exp_body))) -> exp_body in
																										let retval = evaluateExpression so subenvironment sn1 classmap implmap parentmap method_body in
																										let updatestackcount = finishstack stacks in
																										retval
		| Static_dispatch (_, e0, cast_type, methodId, exps_option, _) -> let newstackcount = newstack stacks in
																																			let status_code = match newstackcount < 1000 with
																																													false -> let line_number = getExpressionLineNumber exp in
																																																	 let status = Printf.printf("ERROR: %s: Exception: stack overflow\n") line_number in
																																																	 let exit_status = Pervasives.exit(0) in
																																																	 ""
																																												| true -> ""
																																			in
																																			let (vs, storen) = match exps_option with
																																								None -> ([], store)
																																							| Some(exps) -> List.fold_left (fun acc elem -> let (valelem, storelem) = evaluateExpression so environment (snd acc) classmap implmap parentmap elem in
																																																															let oldvals = fst acc in
																																																															(oldvals @ [valelem], storelem)) ([], store) exps
																																			in
																																			let (v0, sn2) = evaluateExpression so environment storen classmap implmap parentmap e0 in
																																			let status_code = match v0 with
																																													Nulldefault (_) -> let line_number = getExpressionLineNumber e0 in
																																																						 let status = Printf.printf("ERROR: %s: Exception: static dispatch on void\n") line_number in
																																																						 let exit_status = Pervasives.exit(0) in
																																																						 ""
																																												| _ -> ""
																																			in
																																			let target_name = match cast_type with
																																													Identifier (_, type_str) -> type_str
																																			in
																																			let method_name = match methodId with
																																													Identifier (_, x) -> x
																																			in
																																			let method_lookedup = ImplementationMap.find (ClassMethod (target_name, method_name)) implmap in
																																			let arguments_list = match method_lookedup with
																																														 (Method (_, _, argslist, _)) -> argslist in
																																			let newlocations = List.fold_left (fun acc elem -> let oneloc = newloc nextloc in
																																																							acc @ [Location (oneloc)]) [] arguments_list in
																																			let sn3 = List.fold_left2 (fun acc elem1 elem2 -> LocationMap.add elem1 elem2 acc) sn2 newlocations vs in
																																			let subenvironment = let fieldlist = List.fold_left2 (fun acc elem1 elem2 -> let variable_field = match elem1 with
																																																																																					(Formal (arg_str)) -> arg_str
																																																																										in
																																																																										acc @ [Field (variable_field, elem2)]) [] arguments_list newlocations
																																													 in
																																													 let attributes_list = match v0 with
																																																									Object (_, fieldslist, _) -> fieldslist
																																																								| _ -> []
																																													 in
																																													 let empty_map = EnvironmentMap.empty in
																																													 let half_map = List.fold_left (fun acc elem -> match elem with
																																																														(Field (field_var, field_loc)) -> EnvironmentMap.add (Variable (field_var)) field_loc acc) empty_map attributes_list
																																													 in
																																													 List.fold_left (fun acc elem -> let elemtuple = match elem with
																																																																						Field (attrname, loc) -> (Variable (attrname), loc)
																																																													 in
																																																													 EnvironmentMap.add (fst elemtuple) (snd elemtuple) acc) half_map fieldlist
																																			in
																																			let method_body = match method_lookedup with
																																											(Method (_, _, _, Some(exp_body))) -> exp_body in
																																			let retval = evaluateExpression v0 subenvironment sn3 classmap implmap parentmap method_body in
																																			let updatestackcount = finishstack stacks in
																																			retval
		| If (_, pred, then_clause, else_clause, _) -> let (v1, store2) = evaluateExpression so environment store classmap implmap parentmap pred in
																									 (match v1 with
																									    Bool (true) -> evaluateExpression so environment store2 classmap implmap parentmap then_clause
																									  | Bool (false) -> evaluateExpression so environment store2 classmap implmap parentmap else_clause)
		| Block (_, exps_option, _) -> (match exps_option with
																		  Some (exps) -> List.fold_left (fun acc elem -> evaluateExpression so environment (snd acc) classmap implmap parentmap elem) (Nulldefault (true), store) exps)
		| Let_exp (_, bindings, body, _) -> let (environmentn, storen) = List.fold_left (fun acc elem -> match elem with
																																																			 Let_binding (var, typeid, Some(exp)) -> let (v1, s2) = evaluateExpression so (fst acc) (snd acc) classmap implmap parentmap exp in
																																																			 																				let l1 = Location (newloc nextloc) in
																																																																							 let s3 = LocationMap.add l1 v1 s2 in
																																																																							 let var_str = match var with
																																																																							 								Identifier (_, str_var) -> str_var
																																																																							 in
																																																																							 let idvar = Variable (var_str) in
																																																																							 let environmentprime = EnvironmentMap.add idvar l1 (fst acc) in
																																																																							 (environmentprime, s3)
																																																		 | Let_binding (var, typeid, None) -> let type_str = match typeid with
																																																		 																										  Identifier (_, str_type) -> str_type
																																																																					in
																																																																					let v1 = match type_str with
																																																																							 			"Int" -> Int (Int32.of_int 0)
																																																																									 | "Bool" -> Bool (false)
																																																																									 | "String" -> String ("", 0)
																																																																									 | _ -> Nulldefault (true)
																																																																					in
																																																																					let l1 = Location (newloc nextloc) in
																																																																					let s2 = LocationMap.add l1 v1 (snd acc) in
																																																																					let var_str = match var with
																																																																					 	 						Identifier (_, str_var) -> str_var
																																																																					in
																																																																					let idvar = Variable (var_str) in
																																																																					let environmentprime = EnvironmentMap.add idvar l1 (fst acc) in
																																																																					(environmentprime, s2)
																																										) (environment, store) bindings
																				in
																				evaluateExpression so environmentn storen classmap implmap parentmap body
		| Case_exp (_, e0, cases, _) -> let (v0, s2) = evaluateExpression so environment store classmap implmap parentmap e0 in
																		let class_name = match v0 with
																												Bool (_) -> "Bool"
																											| Int (_) -> "Int"
																											| String (_, _) -> "String"
																											| Object (x, _, _) -> x
																											| Nulldefault (_) -> let line_number = getExpressionLineNumber e0 in
																																					 let status = Printf.printf("ERROR: %s: Exception: case on void\n") line_number in
																																					 let exit_status = Pervasives.exit(0) in
																																					 ""
																		in
																		let case_classes = List.fold_left (fun acc elem -> let type_id = match elem with
																																												 						 Case_element (_, typeid, _) -> typeid
																																											 in
																																											 let type_str = match type_id with
																																											 								 Identifier (_, type_str) -> type_str
																																											 in
																																											 type_str :: acc) [] cases
																		in
																		let branch_type = findLeastCommonAncestor class_name case_classes parentmap in
																		let test_branch = match branch_type with
																												"" -> let line_number = getExpressionLineNumber e0 in
																															let status = Printf.printf("ERROR: %s: Exception: no matching branch in case\n") line_number in
																															let exit_status = Pervasives.exit(0) in
																															""
																										  | _ -> ""
																		in
																		let branch = List.hd (List.filter (fun each -> let type_id = match each with
																																																	 Case_element (_, typeid, _) -> typeid
																																									 in
																																									 let type_str = match type_id with
																																													 				 Identifier (_, type_str) -> type_str
																																									 in
																																									 type_str = branch_type) cases) in
																		let l0 = Location (newloc nextloc) in
																		let s3 = LocationMap.add l0 v0 s2 in
																		let var_str = let var_id = match branch with
																																 Case_element (varid, _, _) -> varid
																									in
																									match var_id with
																										Identifier (_, varstr) -> varstr
																		in
																		let environmentprime = EnvironmentMap.add (Variable (var_str)) l0 environment in
																	  (match branch with
																		 	Case_element (_, _, case_exp) -> evaluateExpression so environmentprime s3 classmap implmap parentmap case_exp)
		| While (_, pred, body, _) as whileself -> let (pred_eval, s2) = evaluateExpression so environment store classmap implmap parentmap pred in
																							 (match pred_eval with
																							    Bool (true) -> let (v2, s3) = evaluateExpression so environment s2 classmap implmap parentmap body in
																								 	 						 let (v3, s4) = evaluateExpression so environment s3 classmap implmap parentmap whileself in
																								 								(Nulldefault (false), s4)
																							  | Bool (false) -> (Nulldefault (false), s2))
		| Isvoid (_, body_expr, _) -> let (exprval, s2) = evaluateExpression so environment store classmap implmap parentmap body_expr in
																	(match exprval with
																	 	Nulldefault (_) -> (Bool (true), s2)
																	 | _ -> (Bool (false), s2))
		| Not (_, body_expr, _) -> let (expval, s2) = evaluateExpression so environment store classmap implmap parentmap body_expr in
															 (match expval with
															 	 Bool (true) -> (Bool (false), s2)
															  | Bool (false) -> (Bool (true), s2))
		| Negate (_, body_expr, _) -> let (expval, s2) = evaluateExpression so environment store classmap implmap parentmap body_expr in
																	(match expval with
																	 	Int (x) -> (Int (Int32.neg x), s2))
		| Plus (_, expr1, expr2, _) -> let (e1, s2) = evaluateExpression so environment store classmap implmap parentmap expr1 in
																	 let (e2, s3) = evaluateExpression so environment s2 classmap implmap parentmap expr2 in
																	 (match (e1, e2) with
																	    (Int (x), Int (y)) -> (Int (Int32.add x y), s3))
		| Minus (_, expr1, expr2, _) -> let (e1, s2) = evaluateExpression so environment store classmap implmap parentmap expr1 in
																		let (e2, s3) = evaluateExpression so environment s2 classmap implmap parentmap expr2 in
																		(match (e1, e2) with
																		 	(Int (x), Int (y)) -> (Int (Int32.sub x y), s3))
		| Times (_, expr1, expr2, _) -> let (e1, s2) = evaluateExpression so environment store classmap implmap parentmap expr1 in
																		let (e2, s3) = evaluateExpression so environment s2 classmap implmap parentmap expr2 in
																		(match (e1, e2) with
																		 	(Int (x), Int (y)) -> (Int (Int32.mul x y), s3))
		| Divide (_, expr1, expr2, _) -> let (e1, s2) = evaluateExpression so environment store classmap implmap parentmap expr1 in
																		 let (e2, s3) = evaluateExpression so environment s2 classmap implmap parentmap expr2 in
																		 (match (e1, e2) with
																		    (Int (x), Int (y)) -> match Int32.compare y (Int32.of_int 0) with
																																0 -> let line_number = getExpressionLineNumber expr2 in
																																		 let status = Printf.printf("ERROR: %s: Exception: division by zero\n") line_number in
																																		 let exit_status = Pervasives.exit(0) in
																																		 (Int (Int32.of_int 0), s3)
																															| _ -> (Int (Int32.div x y), s3))
		| Lt (_, expr1, expr2, _) -> let (e1, s2) = evaluateExpression so environment store classmap implmap parentmap expr1 in
																 let (e2, s3) = evaluateExpression so environment s2 classmap implmap parentmap expr2 in
																 (match (e1, e2) with
																  	(Int (x), Int (y)) -> (match Int32.compare x y with
																													 	-1 -> (Bool (true), s3)
																													 | _ -> (Bool (false), s3))
																  | (String (x, _), String (y, _)) -> (match String.compare x y with
																																			 	-1 -> (Bool (true), s3)
																																			 | _ -> (Bool (false), s3))
																  | (Bool (x), Bool (y)) -> (match (x, y) with
																   														(false, true) -> (Bool (true), s3)
																	 													| _ -> (Bool (false), s3))
																  | _ -> (Bool (false), s3))
		| Le (le_exp_id, expr1, expr2, return_type_str) -> let lt_exp = Lt (le_exp_id, expr1, expr2, return_type_str) in
																											 let eq_exp = Eq (le_exp_id, expr1, expr2, return_type_str) in
																											 let (e1, s2) = evaluateExpression so environment store classmap implmap parentmap lt_exp in
																											 let (e2, s3) = evaluateExpression so environment s2 classmap implmap parentmap eq_exp in
																											 (match (e1, e2) with
																											    (Bool (false), Bool (false)) -> (Bool (false), s3)
																											  | _ -> (Bool (true), s3))
		| Eq (_, expr1, expr2, _) -> let (e1, s2) = evaluateExpression so environment store classmap implmap parentmap expr1 in
																 let (e2, s3) = evaluateExpression so environment s2 classmap implmap parentmap expr2 in
																 (match (e1, e2) with
																    (Int (x), Int (y)) -> (match Int32.compare x y with
																														 0 -> (Bool (true), s3)
																													 | _ -> (Bool (false), s3))
																  | (String (x, _), String (y, _)) -> (match String.compare x y with
																																			 	0 -> (Bool (true), s3)
																																			 | _ -> (Bool (false), s3))
																  | (Bool (x), Bool (y)) -> (Bool (x = y), s3)
																  | (Nulldefault (_), Nulldefault (_)) -> (Bool (true), s3)
																  | (Object (_, _, x), Object (_, _, y)) -> let loc1 = match x with
																																												 Location (num) -> num
																																						in
																																						let loc2 = match y with
																																												 Location (num) -> num
																																						in
																																						(Bool (loc1 = loc2), s3)
																  | _ -> (Bool (false), s3))
		| Internal_exp(_, builtinmethod, _) -> let return_val = match builtinmethod with
																						 								"Object.abort" -> let status = Printf.printf("abort\n") in
																														  								 (Int (Obj.magic Pervasives.exit(0)), store)
																													 | "Object.type_name" -> let class_name = match so with
																																																			Bool (_) -> "Bool"
																																																	  | Int (_) -> "Int"
																																																	  | String (_, _) -> "String"
																																																	  | Object (x, _, _) -> x
																																									 in
																																									 (String (class_name, String.length class_name), store)
																													 | "Object.copy" -> let copied = match so with
																													 																	Bool (x) -> (Bool (x), store)
																																													 | Int (x) -> (Int (x), store)
																																													 | String (x, y) -> (String (x, y), store)
																																													 | Object (x, y, _) -> let attributes = ClassMap.find x classmap in
																																													  									let (copied_list, newstore) = List.fold_left2 (fun acc elem1 elem2 ->
																																																																																				let var_name = match elem1 with
																																																																																												 Attribute (varstr, _, _) -> varstr
																																																																																				in
																																																																																				let corresponding_loc = Location (newloc nextloc)
																																																																																				in
																																																																																				let accstore = snd acc in
																																																																																				let acclist = fst acc in
																																																																																				let orig_loc = match elem2 with
																																																																																															Field (_, loc) -> loc
																																																																																				in
																																																																																				let orig_val = LocationMap.find orig_loc store in
																																																																																				let updatestore = LocationMap.add corresponding_loc orig_val accstore in
																																																																																				(acclist @ [Field (var_name, corresponding_loc)], updatestore)
																																																																																				) ([], store) attributes y
																																																							in
																																																							(Object (x, copied_list, Location (newloc nextloc)), newstore)
																																							in
																																							copied
																													 | "IO.out_string" -> let output_string_location = EnvironmentMap.find (Variable ("x")) environment in
																																								let output_string = match LocationMap.find output_string_location store with
																																																			String (str, _) -> str
																																								in
																																								let newlinereg = Str.regexp "\\\\n" in
																																								let replacednewline = Str.global_replace newlinereg "\n" output_string in
																																								let tabreg = Str.regexp "\\\\t" in
																																								let final_string = Str.global_replace tabreg "\t" replacednewline in
																																								let status = print_string final_string in
																																								(so, store)
																													 | "IO.out_int" -> let output_int_location = EnvironmentMap.find (Variable ("x")) environment in
																																						 let output_int = match LocationMap.find output_int_location store with
																																																	 Int (intval) -> intval
																																						 in
																																						 let status = Printf.printf("%ld") output_int in
																																						 (so, store)
																													 | "IO.in_string" -> let read_str =
																													 											try
																																								 	read_line ()
																																								 with End_of_file ->
																																								   ""
																																							 in
																													 									  let null_contain = ref false in
																													    								 for index=0 to ((String.length read_str) - 2)
																																							 do
																																							   if Char.code (String.get read_str index) = 0
																																								 then
																																								 	null_contain := true
																																							 done;
																																							 (match !null_contain with
																													 									   	false -> (String (read_str, String.length read_str), store)
																																							  | true -> (String ("", 0), store))
																													 | "IO.in_int" -> let read_str =
																													 									 try
																																								read_line ()
																																							with End_of_file ->
																																							  ""
																																						in
																																						let start_index = ref 0 in
																																						let empty_flag = ref true in
																																						for index=1 to String.length read_str
																																						do
																																							if !empty_flag then
																																								if String.get read_str (index - 1) = ' ' || String.get read_str (index -1 ) = '\t' then
																																									start_index := index
																																								else
																																									empty_flag := false
																																						done;
																																						let max_length = ref 0 in
																																						let add_flag = ref true in
																																						for index=(!start_index + 1) to String.length read_str
																																						do
																																							let digit_ascii = Char.code (String.get read_str (index - 1)) in
																																							if digit_ascii < 48 || digit_ascii > 57 then
																																								if index=(!start_index + 1) && (String.get read_str (index - 1)) = '-' then
																																									max_length := 1
																																								else
																																									add_flag := false
																																							else
																																								if !add_flag then
																																									max_length := !max_length + 1
																																						done;
																																						let number =
																																							try
																																								int_of_string (String.sub read_str !start_index !max_length)
																																							with Failure "int_of_string" ->
																																								0
																																						in
																																						(match (number < -2147483648, number > 2147483647) with
																																						 	(false, false) -> (Int (Int32.of_int number), store)
																																						 | (_, _) -> (Int (Int32.of_int 0)), store)
																													 | "String.length" -> let str_length = match so with
																													 																 			 String (_, length) -> length
																																								in
																																								(Int (Int32.of_int str_length), store)
																													 | "String.concat" -> let var_string_location = EnvironmentMap.find (Variable ("s")) environment in
																																								let var_string = match LocationMap.find var_string_location store with
																																																			String (str, _) -> str
																																								in
																																								let orig_string = match so with
																																										                String (str, _) -> str
																																								in
																																								(String (orig_string ^ var_string, String.length (orig_string ^ var_string)), store)
																													 | "String.substr" -> let orig_string = match so with
																																																		String (str, _) -> str
																																								in
																																								let i_index_location = EnvironmentMap.find (Variable ("i")) environment in
																																								let i_index = match LocationMap.find i_index_location store with
																																																Int (intval) -> Int32.to_int intval
																																								in
																																								let l_index_location = EnvironmentMap.find (Variable ("l")) environment in
																																								let l_index = match LocationMap.find l_index_location store with
																																																Int (intval) -> Int32.to_int intval
																																								in
																																								let total_length = String.length orig_string in
																																								match (i_index < total_length && i_index > -1, (i_index + l_index - 1) < total_length && l_index > -1) with
																																									(true, true) -> (String (String.sub orig_string i_index l_index, l_index), store)
																																								| (_, _) -> let status = Printf.printf("ERROR: 0: Exception: substr out of range\n") in
																																														let exit_status = Pervasives.exit(0) in
																																														(String ("", 0), store)
																					 in
																					 return_val

(*Parsing inputs*)
let processClassMap inputs_str =
	let content = List.tl inputs_str in
  let cls_count = int_of_string (List.hd content) in
  let rec findAttributes prog_str attr_counts = match attr_counts with
																						 0 -> ([], prog_str)
       																		| 1 -> (match prog_str with
 	           												               "no_initializer" :: attr_name :: type_name :: lst -> ([Attribute (attr_name, type_name, None)], lst)
 						 																		 | "initializer" :: attr_name :: type_name :: lst -> let init_exp = processExpression lst in
																																																			([Attribute (attr_name, type_name, Some(fst init_exp))], snd init_exp))
     																			| _ -> let first_attr = findAttributes prog_str 1 in
																					 			 let rest_attr = findAttributes (snd first_attr) (attr_counts - 1) in
																									((fst first_attr) @ (fst rest_attr), snd rest_attr)
  in
	let findClass cls_str = let class_name = List.hd cls_str in
													let attr_counts = int_of_string (List.hd (List.tl cls_str)) in
													let class_attributes = findAttributes (drop 2 cls_str) attr_counts in
													((class_name, fst class_attributes), snd class_attributes)
	in
  let rec findClasses cls_str counts = match counts with
       																	1 -> let first_class = findClass cls_str in
																				      ([fst first_class], snd first_class)
     															  	| _ -> let first_class = findClass cls_str in
																			   		 let more_classes = findClasses (snd first_class) (counts - 1) in
																							((fst first_class) :: (fst more_classes), snd more_classes)
	in
	let class_lists = findClasses (List.tl content) cls_count in
	let class_map_return = ClassMap.empty in
	let result_map = List.fold_left (fun acc elem -> ClassMap.add (fst elem) (snd elem) acc) class_map_return (fst class_lists) in
	(result_map, snd class_lists)

let processImplementationMap inputs_str =
	let content_with_count = List.tl inputs_str in
	let cls_count = int_of_string (List.hd content_with_count) in
	let rec findFormals prog_str formal_counts = match formal_counts with
																							  0 -> ([], prog_str)
																							| 1 -> ([Formal (List.hd prog_str)], List.tl prog_str)
																							| _ -> let rest_formals = findFormals (List.tl prog_str) (formal_counts - 1) in
																							 			((Formal (List.hd prog_str)) :: (fst rest_formals), snd rest_formals)
	in
	let rec findMethods prog_str method_counts = match method_counts with
																							  1 -> let method_name = List.hd prog_str in
																										 let formal_counts = int_of_string (List.hd (List.tl prog_str)) in
																										 let formal_lists_rest = findFormals (drop 2 prog_str) formal_counts in
																										 let defined_class = List.hd (snd formal_lists_rest) in
																										 let method_body = processExpression (List.tl (snd formal_lists_rest)) in
																										 ([Method (method_name, defined_class, (fst formal_lists_rest), Some(fst method_body))], snd method_body)
																							| _ -> let first_method = findMethods prog_str 1 in
																										 let rest_methods = findMethods (snd first_method) (method_counts - 1) in
																										 ((fst first_method) @ (fst rest_methods), snd rest_methods)
	in
	let findClassMethod cls_str = let class_name = List.hd cls_str in
																let method_counts = int_of_string (List.hd (List.tl cls_str)) in
																let class_method = findMethods (drop 2 cls_str) method_counts in
																((class_name, fst class_method), snd class_method)
	in
	let rec findClassesMethods cls_str counts = match counts with
																								1 -> let first_class = findClassMethod cls_str in
																										 ([fst first_class], snd first_class)
																							| _ -> let first_class = findClassMethod cls_str in
																										 let more_classes = findClassesMethods (snd first_class) (counts - 1) in
																										 ((fst first_class) :: (fst more_classes), snd more_classes)
	in
	let class_lists = findClassesMethods (List.tl content_with_count) cls_count in
	let impl_map_return = ImplementationMap.empty in
	let result_map = List.fold_left (fun acc elem -> let methodelem = snd elem in
																									 List.fold_left (fun acc2 elem2 -> let methodelemname = match elem2 with
																																																						(Method (methodname, _, _, _)) -> methodname
																																										 in
																																										 ImplementationMap.add (ClassMethod (fst elem, methodelemname)) elem2 acc2) acc methodelem) impl_map_return (fst class_lists)
	in
	(result_map, snd class_lists)

let processParentMap inputs_str =
	let content_with_count = List.tl inputs_str in
	let count = int_of_string (List.hd content_with_count) in
	let rec findChildParent prog_str counts_left = match counts_left with
																									 1 -> let childClass = List.hd prog_str in
																									      let parentClass = List.hd (List.tl prog_str) in
																												([(childClass, parentClass)], drop 2 prog_str)
																								 | _ -> let firstPair = findChildParent prog_str 1 in
																								 			 let rest_pairs = findChildParent (snd firstPair) (counts_left - 1) in
																												((fst firstPair) @ (fst rest_pairs), snd rest_pairs)
	in
	let pairs = findChildParent (drop 2 inputs_str) count in
	let parent_map = ParentMap.empty in
	let result_map = List.fold_left (fun acc elem -> ParentMap.add (fst elem) (snd elem) acc) parent_map (fst pairs) in
	(result_map, snd pairs)

(*Initialization before evaluation starts. (new Main).main() *)
let initialization classmap implmap parentmap =
										 let newstackcount = newstack stacks in
										 let attributes_list = match ClassMap.find "Main" classmap with
										  											 [] -> [Attribute ("", "", None)]
																					 | lst -> lst
										 in
										 let newlocations = List.fold_left (fun acc elem -> let oneloc = newloc nextloc in
																																				acc @ [Location (oneloc)]) [] attributes_list in
										 let newemptyfield = List.fold_left2 (fun acc elem1 elem2 -> let attribute_name = match elem1 with
																																									(Attribute (attrname, _, _)) -> attrname
																																								 in
																																								 acc @ [Field (attribute_name, elem2)]) [] attributes_list newlocations
										 in
										 let newemptyvalue = Object ("Main", newemptyfield, Location (newloc nextloc)) in
										 let store2 = List.fold_left2 (fun acc elem1 elem2 -> let attribute_type = match elem1 with
																																						(Attribute (_, typename, _)) -> typename
																																					in
																																					match attribute_type with
																																						"Int" -> LocationMap.add elem2 (Int (Int32.of_int 0)) acc
																																					| "String" -> LocationMap.add elem2 (String ("", 0)) acc
																																					| "Bool" -> LocationMap.add elem2 (Bool (false)) acc
																																					| _ -> LocationMap.add elem2 (Nulldefault (true)) acc) LocationMap.empty attributes_list newlocations
										 in
										 let subenvironment = List.fold_left2 (fun acc elem1 elem2 -> let attribute_name = match elem1 with
																																																				 (Attribute (attrname, _, _)) -> attrname
																																									in
																																									EnvironmentMap.add (Variable (attribute_name)) elem2 acc) EnvironmentMap.empty attributes_list newlocations
										 in
										 let store3 = List.fold_left2 (fun acc elem1 elem2 -> match elem1 with
																																						(Attribute (_, _, None)) -> acc
																																					| (Attribute (_, _, Some(e))) -> let (elemval, storenew) = evaluateExpression newemptyvalue subenvironment acc classmap implmap parentmap e
																																																					 in
																																																					 LocationMap.add elem2 elemval storenew) store2 attributes_list newlocations
  								   in
										 let updatestackcount = finishstack stacks in
										 (newemptyvalue, subenvironment, store3)

(*Program starts here*)
let () =
  let inputs = read_file Sys.argv.(1) in
	let (classmap, remain) = processClassMap inputs in
	let (implmap, remain2) = processImplementationMap remain in
	let (parentmap, remain3) = processParentMap remain2 in
	let main_method = ImplementationMap.find (ClassMethod ("Main", "main")) implmap in
	let (self_object, environment, store) = initialization classmap implmap parentmap in
	let main_exp = match main_method with
									 Method (_, _, _, Some(exp)) -> exp
	in
	let evaluated = evaluateExpression self_object environment store classmap implmap parentmap main_exp in
	match evaluated with
		_ -> Pervasives.exit(0)
