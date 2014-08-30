-- Zihao Wang
-- zw2rf@virginia.edu
-- March 18, 2014
-- PA4 submission

-- imports

import qualified Data.List as List
import qualified Data.Map as Map
import Data.String
import Data.Bool
import System.Environment
import System.IO

-- constants

reservedInheritClasses = ["String", "Int", "Bool", "SELF_TYPE"] -- classes that cannot be inherited
reservedClasses = ["String", "Int", "Bool", "SELF_TYPE", "Object", "IO"] -- classes that cannot be redefiend
knownTypes = ["String", "IO", "Bool", "SELF_TYPE", "Object", "Int"] -- known types

-- built-in classes from the cool reference manual

objectFeature = [Method { method_identifier = Identifier { id_name = "abort",
                                                           line_number = "0"
                                                         },
                          method_return_type = Identifier { id_name = "Object",
                                                            line_number = "0"
                                                          },
                          method_formals = Nothing,
                          method_body = Nothing
                        },
                 Method { method_identifier = Identifier { id_name = "copy",
                                                           line_number = "0"
                                                         },
                          method_return_type = Identifier { id_name = "SELF_TYPE",
                                                            line_number = "0"
                                                          },
                          method_formals = Nothing,
                          method_body = Nothing
                        },
                  Method { method_identifier = Identifier { id_name = "type_name",
                                                            line_number = "0"
                                                         },
                          method_return_type = Identifier { id_name = "String",
                                                            line_number = "0"
                                                          },
                          method_formals = Nothing,
                          method_body = Nothing
                        }
                 ]

objectClass = Class { class_identifier = Identifier { id_name = "Object"
                                                    , line_number = "0"  
                                                    },
                      inherits_type = Nothing,
                      class_features = Just objectFeature
                    }
              
ioFeature = [Method { method_identifier = Identifier { id_name = "in_int",
                                                       line_number = "0"
                                                     },
                      method_return_type = Identifier { id_name = "Int",
                                                        line_number = "0"
                                                      },
                      method_formals = Nothing,
                      method_body = Nothing
                    },
             Method { method_identifier = Identifier { id_name = "in_string",
                                                       line_number = "0"
                                                     },
                      method_return_type = Identifier { id_name = "String",
                                                        line_number = "0"
                                                      },
                      method_formals = Nothing,
                      method_body = Nothing
                    },
             Method { method_identifier = Identifier { id_name = "out_int",
                                                       line_number = "0"
                                                     },
                      method_return_type = Identifier { id_name = "SELF_TYPE",
                                                        line_number = "0"
                                                      },
                      method_formals = Just [Formal { formal_identifier = Identifier { id_name = "x",
                                                                                       line_number = "0"
                                                                                     },
                                                      formal_type_name = Identifier { id_name = "Int",
                                                                                      line_number ="0"
                                                                                    }
                                                    }
                                            ],
                      method_body = Nothing
                    },
             Method { method_identifier = Identifier { id_name = "out_string",
                                                       line_number = "0"
                                                     },
                      method_return_type = Identifier { id_name = "SELF_TYPE",
                                                        line_number = "0"
                                                      },
                      method_formals = Just [Formal { formal_identifier = Identifier { id_name = "x",
                                                                                       line_number = "0"
                                                                                     },
                                                      formal_type_name = Identifier { id_name = "String",
                                                                                      line_number ="0"
                                                                                    }
                                                    }
                                            ],
                      method_body = Nothing
                      }
            ]
            
ioClass = Class { class_identifier = Identifier { id_name = "IO",
                                                   line_number = "0"  
                                                },
                      inherits_type = Just (Identifier { id_name = "Object",
                                                         line_number = "0"
                                                       }),
                      class_features = Just ioFeature
                }
          
stringFeature = [Method { method_identifier = Identifier { id_name = "concat",
                                                           line_number = "0"
                                                         },
                          method_return_type = Identifier { id_name = "String",
                                                            line_number = "0"
                                                          },
                          method_formals = Just [Formal { formal_identifier = Identifier { id_name = "s",
                                                                                           line_number = "0"
                                                                                         },
                                                          formal_type_name = Identifier { id_name = "String",
                                                                                          line_number ="0"
                                                                                        }
                                                        }
                                                ],
                          method_body = Nothing
                        },
                 Method { method_identifier = Identifier { id_name = "length",
                                                           line_number = "0"
                                                         },
                          method_return_type = Identifier { id_name = "Int",
                                                            line_number = "0"
                                                          },
                          method_formals = Nothing,
                          method_body = Nothing
                        },
                  Method { method_identifier = Identifier { id_name = "substr",
                                                           line_number = "0"
                                                          },
                          method_return_type = Identifier { id_name = "String",
                                                            line_number = "0"
                                                          },
                          method_formals = Just [Formal { formal_identifier = Identifier { id_name = "i",
                                                                                           line_number = "0"
                                                                                         },
                                                          formal_type_name = Identifier { id_name = "Int",
                                                                                          line_number ="0"
                                                                                        }
                                                        },
                                                 Formal { formal_identifier = Identifier { id_name = "l",
                                                                                           line_number = "0"
                                                                                         },
                                                          formal_type_name = Identifier { id_name = "Int",
                                                                                          line_number ="0"
                                                                                        }
                                                        }
                                                ],
                          method_body = Nothing
                        }
                 ]
stringClass = Class { class_identifier = Identifier { id_name = "String",
                                                   line_number = "0"  
                                                },
                      inherits_type = Just (Identifier { id_name = "Object",
                                                         line_number = "0"
                                                       }),
                      class_features = Just stringFeature
                    }

intClass = Class { class_identifier = Identifier { id_name = "Int",
                                                   line_number = "0"  
                                                },
                      inherits_type = Just (Identifier { id_name = "Object",
                                                         line_number = "0"
                                                       }),
                      class_features = Nothing
                 }

boolClass = Class { class_identifier = Identifier { id_name = "Bool",
                                                   line_number = "0"  
                                                },
                      inherits_type = Just (Identifier { id_name = "Object",
                                                         line_number = "0"
                                                       }),
                      class_features = Nothing
                  }
            
-- data structures

data TypeCool = DirectType { type_id :: String } | SelfType { type_id :: String } -- this type is for the type in cool, it can be either a SELF_TYPE of a normal type.

-- cool expressions
data Expression = Assign { exp_identifier :: Identifier
                         , var :: Identifier
                         , rhs :: Expression
                         }
                  | Dynamic_dispatch { exp_identifier :: Identifier
                                     , e :: Expression
                                     , method :: Identifier
                                     , args :: Maybe [Expression]
                                     }
                  | Static_dispatch { exp_identifier :: Identifier
                                    , e :: Expression
                                    , type_cast :: Identifier
                                    , method :: Identifier
                                    , args :: Maybe [Expression]
                                    }
                  | Self_dispatch { exp_identifier :: Identifier
                                  , method :: Identifier
                                  , args :: Maybe [Expression]
                                  }
                  | If { exp_identifier :: Identifier
                       , predicate :: Expression
                       , then_exp :: Expression
                       , else_exp :: Expression
                       }
                  | While { exp_identifier :: Identifier
                          , predicate :: Expression
                          , while_body :: Expression
                          }
                  | Block { exp_identifier :: Identifier
                          , block_body :: Maybe [Expression]
                          }
                  | New { exp_identifier :: Identifier
                        , class_id :: Identifier
                        }
                  | Isvoid { exp_identifier :: Identifier
                           , e :: Expression
                           }
                  | Plus { exp_identifier :: Identifier
                         , x :: Expression
                         , y :: Expression
                         }
                  | Minus { exp_identifier :: Identifier
                          , x :: Expression
                          , y :: Expression
                          }
                  | Times { exp_identifier :: Identifier
                          , x :: Expression
                          , y :: Expression
                          }
                  | Divide { exp_identifier :: Identifier
                           , x :: Expression
                           , y :: Expression
                           }
                  | Lt { exp_identifier :: Identifier
                       , x :: Expression
                       , y :: Expression
                       }
                  | Le { exp_identifier :: Identifier
                       , x :: Expression
                       , y :: Expression
                       }
                  | Eq { exp_identifier :: Identifier
                       , x :: Expression
                       , y :: Expression
                       }
                  | Not { exp_identifier :: Identifier
                        , x :: Expression
                        }
                  | Negate { exp_identifier :: Identifier
                           , x :: Expression
                           }
                  | Integer_exp { exp_identifier :: Identifier
                                , int_constant :: Int
                                }
                  | String_exp { exp_identifier :: Identifier
                               , string_constant :: String
                               }
                  | Identifier_exp { exp_identifier :: Identifier
                                   , id_exp :: Identifier
                                   }
                  | Bool_exp { exp_identifier :: Identifier
                             , bool_constant :: Bool
                             }
                  | Let_exp { exp_identifier :: Identifier
                            , bindings_list :: [Let_binding]
                            , body_exp :: Expression
                            }
                  | Case_exp { exp_identifier :: Identifier
                             , case_body_exp :: Expression
                             , case_element_list :: [Case_element]
                             } deriving (Show)
                                        
data Let_binding = Let_binding { binding_variable :: Identifier
                               , binding_type_id :: Identifier
                               , binding_init_exp :: Maybe Expression
                               } deriving (Show)

data Case_element = Case_element { case_variable :: Identifier
                                 , case_type_id :: Identifier
                                 , case_element_body :: Expression
                                 } deriving (Show)

data Identifier = Identifier { id_name :: String
                             , line_number :: String
                             } deriving (Show)

-- other data types that represent part of the cool language

data Formal = Formal { formal_identifier :: Identifier
                     , formal_type_name :: Identifier
                     } deriving (Show)


data Feature = Attribute { attribute_identifier :: Identifier
                        , attribute_type_name :: Identifier
                        , init_expression :: Maybe Expression
                        }
              | Method { method_identifier :: Identifier
                       , method_return_type :: Identifier
                       , method_formals :: Maybe [Formal]
                       , method_body :: Maybe Expression
                       } deriving (Show)
                                 
data Class = Class { class_identifier :: Identifier
                   , inherits_type :: Maybe Identifier
                   , class_features :: Maybe [Feature]
                   } deriving (Show)

-- Program is just a list of Classes
type Program = [Class]

-- funcitons that process inputs

processIdentifier :: [String] -> Identifier
processIdentifier input = Identifier {line_number = input !! 0, id_name = input !! 1}

processMethodFormals :: [String] -> (Maybe [Formal], Identifier)
processMethodFormals input = let count = read $ head input :: Int in
                             case count of 0 -> (Nothing, processIdentifier $ tail input)
                                           _ -> (Just (findrec2 count $ tail input), processIdentifier $ drop (4 * count + 1) input)
                                           where findrec2 x y
                                                         | x == 0 = []
                                                         | x > 0 = [Formal {formal_identifier = processIdentifier y, formal_type_name = processIdentifier $ drop 2 y}] ++ (findrec2 (x - 1) $ drop 4 y)

processMethodBody :: [String] -> (Expression, [String])
processMethodBody input = let count = read $ head input :: Int in
                          processExpression $ drop (4 * count + 3) input

processFeatures :: [String] -> (Maybe [Feature], [String])
processFeatures input = let count = read $ head input :: Int in
                        case count of 0 -> (Nothing, tail input)
                                      _ -> let result = findFeatures (tail input) count in
                                           (Just (fst result), snd result)
                                           where findFeatures x y = case y of 1 -> let first_feature = processFeature x in
                                                                                   ([fst first_feature], snd first_feature)
                                                                              _ -> let first_feature = processFeature x in
                                                                                   let rest_feature = findFeatures (snd first_feature) (y-1) in
                                                                                   ([fst first_feature] ++ (fst rest_feature), snd rest_feature)

processFeature :: [String] -> (Feature, [String])
processFeature ("attribute_no_init":lst) = (Attribute {attribute_identifier = processIdentifier lst, attribute_type_name = processIdentifier $ drop 2 lst, init_expression = Nothing}, drop 4 lst)
processFeature ("attribute_init":lst) = let init_exp_remain = processExpression $ drop 4 lst in
                                        (Attribute {attribute_identifier = processIdentifier lst, attribute_type_name = processIdentifier $ drop 2 lst, init_expression = Just (fst init_exp_remain)}, snd init_exp_remain)
processFeature ("method":lst) = let processed_method = processMethodFormals $ drop 2 lst
                                    processed_body = processMethodBody $ drop 2 lst
                                in
                                (Method {method_identifier = processIdentifier lst, method_return_type = snd processed_method, method_formals = fst processed_method, method_body = Just (fst processed_body)}, snd processed_body)

processCaseElement :: [String] -> (Case_element, [String])
processCaseElement lst = let this_variable = processIdentifier lst in
                         let this_type = processIdentifier $ drop 2 lst in
                         let exp_remain = processExpression $ drop 4 lst in
                         (Case_element { case_variable = this_variable, case_type_id = this_type, case_element_body = fst exp_remain}, snd exp_remain)

processCaseElements :: [String] -> ([Case_element], [String])
processCaseElements lst = let count = read $ head lst :: Int in
                          getCaseElements (tail lst) count
                          where getCaseElements x y = case y of 1 -> let this_case_element = processCaseElement x in
                                                                     ([fst this_case_element], snd this_case_element)
                                                                _ -> let this_case_element = processCaseElement x in
                                                                     let rest_case_element = getCaseElements (snd this_case_element) (y-1) in
                                                                     ([fst this_case_element] ++ (fst rest_case_element), snd rest_case_element)

processLetBinding :: [String] -> (Let_binding, [String])
processLetBinding (hd:lst) = case hd of "let_binding_no_init" -> let this_var = processIdentifier lst in
                                                                   let this_type = processIdentifier $ drop 2 lst in
                                                                   (Let_binding { binding_variable = this_var, binding_type_id = this_type, binding_init_exp = Nothing }, drop 4 lst)
                                        "let_binding_init" -> let this_var = processIdentifier lst in
                                                              let this_type = processIdentifier $ drop 2 lst in
                                                              let this_init = processExpression $ drop 4 lst in
                                                              (Let_binding { binding_variable = this_var, binding_type_id = this_type, binding_init_exp = Just (fst this_init) }, snd this_init)
                                         

processLetBindings :: [String] -> ([Let_binding], [String])
processLetBindings lst = let count = read $ head lst :: Int in
                                     getBindings (tail lst) count
                                     where getBindings x y = case y of 1 -> let this_binding = processLetBinding x in
                                                                            ([fst this_binding], snd this_binding)
                                                                       _ -> let this_binding = processLetBinding x in
                                                                            let rest_binding = getBindings (snd this_binding) (y-1) in
                                                                            ([fst this_binding] ++ (fst rest_binding), snd rest_binding)

processExpressionList :: [String] -> (Maybe [Expression], [String])
processExpressionList lst = let count = read $ head lst :: Int in
                            case count of 0 -> (Nothing, tail lst)
                                          _ -> let result = findExpressionsAndRemain (tail lst) count in
                                               (Just (fst result), snd result)
                                          where findExpressionsAndRemain x y = case y of 1 -> let first_exp = processExpression x in
                                                                                              ([fst first_exp], snd first_exp)
                                                                                         _ -> let first_exp = processExpression x in
                                                                                              let remain_exp = findExpressionsAndRemain (snd first_exp) (y-1) in
                                                                                              ([fst first_exp] ++ (fst remain_exp), snd remain_exp)

processExpression :: [String] -> (Expression, [String])
processExpression (lineno:"assign":lst) = let rhs_exp = processExpression $ drop 2 lst in
                                          (Assign { exp_identifier = Identifier { id_name = "assign", line_number = lineno }, var =  processIdentifier lst , rhs = fst rhs_exp}, snd rhs_exp)
processExpression (lineno:"dynamic_dispatch":lst) = let this_e = processExpression lst in
                                                    let this_method = processIdentifier $ snd this_e in
                                                    let this_exp_list_remain = processExpressionList $ drop 2 $ snd this_e in
                                                    (Dynamic_dispatch { exp_identifier = Identifier { id_name = "dynamic_dispatch", line_number = lineno }, e = fst this_e, method = this_method, args = fst this_exp_list_remain }, snd this_exp_list_remain)
processExpression (lineno:"static_dispatch":lst) = let this_e = processExpression lst in
                                                   let this_type = processIdentifier $ snd this_e in
                                                   let this_method = processIdentifier $ drop 2 $ snd this_e in
                                                   let this_exp_list_remain = processExpressionList $ drop 4 $ snd this_e in
                                                   (Static_dispatch { exp_identifier = Identifier { id_name = "static_dispatch", line_number = lineno}, e = fst this_e, type_cast = this_type, method = this_method, args = fst this_exp_list_remain }, snd this_exp_list_remain)
processExpression (lineno:"self_dispatch":lst) = let this_method = processIdentifier lst in
                                                 let this_exp_list_remain = processExpressionList $ drop 2 lst in
                                                 (Self_dispatch { exp_identifier = Identifier { id_name = "self_dispatch", line_number = lineno}, method = this_method, args = fst this_exp_list_remain }, snd this_exp_list_remain)
processExpression (lineno:"if":lst) = let this_pred = processExpression lst in
                                      let this_then = processExpression $ snd this_pred in
                                      let this_else = processExpression $ snd this_then in
                                      (If { exp_identifier = Identifier { id_name = "if", line_number = lineno}, predicate = fst this_pred, then_exp = fst this_then, else_exp = fst this_else }, snd this_else)
processExpression (lineno:"while":lst) = let this_while = processExpression lst in
                                         let this_body = processExpression $ snd this_while in
                                         (While { exp_identifier = Identifier { id_name = "while", line_number = lineno}, predicate = fst this_while, while_body = fst this_body }, snd this_body)
processExpression (lineno:"block":lst) = let this_exp_list_remain = processExpressionList lst in
                                         (Block { exp_identifier = Identifier { id_name = "block", line_number = lineno}, block_body = fst this_exp_list_remain}, snd this_exp_list_remain)
processExpression (lineno:"new":lst) = (New { exp_identifier = Identifier { id_name = "new", line_number = lineno}, class_id = processIdentifier lst }, drop 2 lst)
processExpression (lineno:"isvoid":lst) = let this_is_void = processExpression lst in
                                          (Isvoid { exp_identifier = Identifier { id_name = "isvoid", line_number = lineno}, e = fst this_is_void}, snd this_is_void)
processExpression (lineno:"plus":lst) = let this_x = processExpression lst in
                                        let this_y = processExpression $ snd this_x in
                                        (Plus { exp_identifier = Identifier { id_name = "plus", line_number = lineno}, x = fst this_x, y = fst this_y}, snd this_y)
processExpression (lineno:"minus":lst) = let this_x = processExpression lst in
                                         let this_y = processExpression $ snd this_x in
                                         (Minus { exp_identifier = Identifier { id_name = "minus", line_number = lineno}, x = fst this_x, y = fst this_y}, snd this_y)
processExpression (lineno:"times":lst) = let this_x = processExpression lst in
                                         let this_y = processExpression $ snd this_x in
                                         (Times { exp_identifier = Identifier { id_name = "times", line_number = lineno}, x = fst this_x, y = fst this_y}, snd this_y)
processExpression (lineno:"divide":lst) = let this_x = processExpression lst in
                                          let this_y = processExpression $ snd this_x in
                                          (Divide { exp_identifier = Identifier { id_name = "divide", line_number = lineno}, x = fst this_x, y = fst this_y}, snd this_y)
processExpression (lineno:"lt":lst) = let this_x = processExpression lst in
                                      let this_y = processExpression $ snd this_x in
                                      (Lt { exp_identifier = Identifier { id_name = "lt", line_number = lineno}, x = fst this_x, y = fst this_y}, snd this_y)
processExpression (lineno:"le":lst) = let this_x = processExpression lst in
                                      let this_y = processExpression $ snd this_x in
                                      (Le { exp_identifier = Identifier { id_name = "le", line_number = lineno}, x = fst this_x, y = fst this_y}, snd this_y)
processExpression (lineno:"eq":lst) = let this_x = processExpression lst in
                                      let this_y = processExpression $ snd this_x in
                                      (Eq { exp_identifier = Identifier { id_name = "eq", line_number = lineno}, x = fst this_x, y = fst this_y}, snd this_y)
processExpression (lineno:"not":lst) = let this_x = processExpression lst in
                                       (Not { exp_identifier = Identifier { id_name = "not", line_number = lineno}, x = fst this_x}, snd this_x)
processExpression (lineno:"negate":lst) = let this_x = processExpression lst in
                                          (Negate { exp_identifier = Identifier { id_name = "negate", line_number = lineno}, x = fst this_x}, snd this_x)
processExpression (lineno:"integer":lst) = (Integer_exp { exp_identifier = Identifier { id_name = "integer", line_number = lineno }, int_constant = read $ head lst :: Int}, tail lst)
processExpression (lineno:"string":lst) = (String_exp { exp_identifier = Identifier { id_name = "string", line_number = lineno }, string_constant = head lst}, tail lst)
processExpression (lineno:"identifier":lst) = let this_id = processIdentifier lst in
                                              (Identifier_exp { exp_identifier = Identifier { id_name = "identifier", line_number = lineno }, id_exp = this_id }, drop 2 lst)
processExpression (lineno:"true":lst) = (Bool_exp { exp_identifier = Identifier { id_name = "true", line_number = lineno}, bool_constant = True}, lst)
processExpression (lineno:"false":lst) = (Bool_exp { exp_identifier = Identifier { id_name = "false", line_number = lineno}, bool_constant = False}, lst)
processExpression (lineno:"let":lst) = let bindings = processLetBindings lst in
                                       let let_body = processExpression $ snd bindings in
                                       (Let_exp { exp_identifier = Identifier { id_name = "let", line_number = lineno}, bindings_list = fst bindings, body_exp = fst let_body}, snd let_body)
processExpression (lineno:"case":lst) = let this_case = processExpression lst in
                                        let this_case_elements = processCaseElements $ snd this_case in
                                        (Case_exp { exp_identifier = Identifier { id_name = "case", line_number = lineno}, case_body_exp = fst this_case, case_element_list = fst this_case_elements}, snd this_case_elements)

-- some helper functions for processing .cl-ast files

getFeatures :: [String] -> Maybe [Feature]
getFeatures input = fst $ processFeatures input

processClass :: [String] -> (Class, [String])
processClass input = let class_name = processIdentifier input
                         inherit_stat = input !! 2
                         remain_list = drop 3 input
                     in  case inherit_stat of "inherits" -> let super_class = processIdentifier remain_list
                                                                features_list_remaining = processFeatures $ drop 2 remain_list
                                                            in (Class { class_identifier = class_name, inherits_type = Just super_class, class_features = fst features_list_remaining}, snd features_list_remaining)
                                              "no_inherits" -> let features_list_remaining = processFeatures remain_list in
                                                               (Class { class_identifier = class_name, inherits_type = Nothing, class_features = fst features_list_remaining}, snd features_list_remaining)

processProgram :: [String] -> Program
processProgram input = let count = read $ head input :: Int in
                       findClasses (tail input) count
                       where findClasses x y = case y of 1 -> [fst $ processClass x]
                                                         _ -> let first_class = processClass x in
                                                              let rest_class = findClasses (snd first_class) (y-1) in
                                                              [fst first_class] ++ rest_class

-- functions that check for errors without evaluating expressions (PA4c)

checkForFormalTypeSelf :: Program -> (Maybe Int, String)
checkForFormalTypeSelf prog = let line = foldl (\acc element -> case acc of Nothing -> checkFormalTypeSelfClass element
                                                                            Just someline -> Just someline) Nothing prog in
                              (line, "illegal type of self in formal list")

checkForFormalUseOfSelf :: Program -> (Maybe Int, String)
checkForFormalUseOfSelf prog = let line = foldl (\acc element -> case acc of Nothing -> checkSelfInFormalClass element
                                                                             Just someline -> Just someline) Nothing prog in
                               (line, "illegal use of self in formal list")

checkForDuplicateFormal :: Program -> (Maybe Int, String)
checkForDuplicateFormal prog = let line = foldl (\acc element -> case acc of Nothing -> checkDuplicateFormalClass element
                                                                             Just someline -> Just someline) Nothing prog in
                               (line, "duplicate formal found")

checkForClassRedef :: Program -> (Maybe Int, String)
checkForClassRedef prog = let line = checkForClassRedefHelper $ List.sortBy sortClass prog in
                          (line, "class redefinition")
                              where checkForClassRedefHelper (x:y:lst) = case (getClassName x == getClassName y) of False -> fst $ checkForClassRedef (y:lst)
                                                                                                                    True -> Just (getClassLineNumber y)
                                    checkForClassRedefHelper (x:[]) = Nothing
                                    checkForClassRedefHelper [] = Nothing

checkForMainClass :: Program -> (Maybe Int, String)
checkForMainClass prog = let line = case mainpresence prog of True -> Nothing
                                                              False -> Just 0
                         in
                         (line, "no main class defined")
                              where mainpresence prog = foldl (\acc elem -> if acc then True else if getClassName elem == "Main" then True else False) False prog

checkForMainMethod :: Program -> (Maybe Int, String) -- This function assumes the presence of Main class
checkForMainMethod prog = let line = case (mainmethodpresence prog) of True -> Nothing
                                                                       False -> Just 0
                          in
                          (line, "no main method found")
                               where mainmethodpresence prog = foldl (\acc elem -> if acc then True else validmainMethod elem) False (getAllFeaturesForClass (getClass prog "Main") prog)

checkForClassReserved :: Program -> (Maybe Int, String)
checkForClassReserved prog = let line = foldl (\acc element -> case acc of Nothing -> case (elem (getClassName element) reservedClasses) of True -> Just (getClassLineNumber element)
                                                                                                                                            False -> Nothing
                                                                           Just someline -> Just someline) Nothing prog in
                             (line, "class from reserved class")
                                                    
checkForInheritReserved :: Program -> (Maybe Int, String)
checkForInheritReserved prog = let line = foldl (\acc element -> case acc of Nothing -> case (getParent element) of Nothing -> Nothing
                                                                                                                    Just par -> case (elem (getIdentifierName par) reservedInheritClasses) of True -> Just (getLineNumber par)
                                                                                                                                                                                              False -> Nothing
                                                                             Just someline -> Just someline) Nothing prog in
                               (line, "inherit from reserved class")

checkForUseOfSelf :: Program -> (Maybe Int, String)
checkForUseOfSelf prog = let line = foldl (\acc element -> case acc of Nothing -> checkSelfClass element
                                                                       Just someline -> Just someline) Nothing prog in
                         (line, "illegal use of self")

checkSelfClass :: Class -> Maybe Int
checkSelfClass cls = foldl (\acc element -> case acc of Nothing -> checkSelf element
                                                        Just someline -> Just someline) Nothing $ filter filterAttribute $ getFeatureForClass cls

-- helper functions for checking invalid types (types that are not defined)

checkForInvalidType :: Identifier -> [String] -> Maybe Int
checkForInvalidType id type_lst = case (elem (getIdentifierName id) type_lst) of False -> Just (getLineNumber id)
                                                                                 True -> Nothing

checkForInvalidTypeFormal :: Formal -> [String] -> Maybe Int
checkForInvalidTypeFormal (Formal _ x) type_lst = checkForInvalidType x type_lst

checkForInvalidTypeFormals :: [Formal] -> [String] -> Maybe Int
checkForInvalidTypeFormals forms type_lst = foldl (\acc element -> case acc of Nothing -> checkForInvalidTypeFormal element type_lst
                                                                               Just someline -> Just someline) Nothing forms

checkForInvalidTypeFeature :: Feature -> [String] -> Maybe Int
checkForInvalidTypeFeature (Attribute _ x _) type_lst = checkForInvalidType x type_lst
checkForInvalidTypeFeature (Method _ x y _) type_lst = case (checkForInvalidType x type_lst) of Just line -> Just line
                                                                                                Nothing -> case y of Nothing -> Nothing
                                                                                                                     Just forms -> checkForInvalidTypeFormals forms type_lst

checkForInvalidTypeFeatures :: [Feature] -> [String] -> Maybe Int
checkForInvalidTypeFeatures feats type_lst = foldl (\acc element -> case acc of Nothing -> checkForInvalidTypeFeature element type_lst
                                                                                Just someline -> Just someline) Nothing feats

checkForInvalidTypeClass :: Class -> [String] -> Maybe Int
checkForInvalidTypeClass (Class _ x y) type_lst = case x of Just parent -> case (checkForInvalidType parent type_lst) of Just line -> Just line
                                                                                                                         Nothing -> case y of Nothing -> Nothing
                                                                                                                                              Just feats -> checkForInvalidTypeFeatures feats type_lst
                                                            Nothing -> case y of Nothing -> Nothing
                                                                                 Just feats -> checkForInvalidTypeFeatures feats type_lst

checkForInvalidTypeProgram :: Program -> (Maybe Int, String)
checkForInvalidTypeProgram prog = let type_lst = findAllValidTypes prog in
                                  let line = foldl (\acc element -> case acc of Nothing -> checkForInvalidTypeClass element type_lst
                                                                                Just someline -> Just someline) Nothing prog
                                  in
                                  (line, "invalid type declaration")

checkCycleClass :: Class -> Program -> Maybe Int
checkCycleClass cls prog = case (checkClassInheritanceTree cls [] prog) of True -> Just 0
                                                                           False -> Nothing

checkForCyclicInheritance:: Program -> (Maybe Int, String)
checkForCyclicInheritance prog = let line = foldl (\acc element -> case acc of Nothing -> checkCycleClass element prog
                                                                               Just someline -> Just someline) Nothing prog in
                                 (line, "cyclic inheritance")

checkDuplicateInClass :: Class -> Maybe Int
checkDuplicateInClass cls = findDuplicateFeature $ getFeatureForClass cls

checkForDuplicateFeature :: Program -> (Maybe Int, String)
checkForDuplicateFeature prog = let line = foldl (\acc element -> case acc of Nothing -> checkDuplicateInClass element
                                                                              Just someline -> Just someline) Nothing prog in
                                (line, "feature redefined")

checkInheritedAttributeDuplicateClass :: Class -> Program -> Maybe Int
checkInheritedAttributeDuplicateClass cls prog = findInheritanceAttributeDuplicate $ map (filter filterAttribute . getFeatureForClass) (getAllInheritance cls prog)

checkForInheritedAttributeDuplicate :: Program -> (Maybe Int, String)
checkForInheritedAttributeDuplicate prog = let line = foldl (\acc element -> case acc of Nothing -> checkInheritedAttributeDuplicateClass element prog
                                                                                         Just someline -> Just someline) Nothing prog in
                                           (line, "redefinition of attribute from super class")

checkOverideClass :: Class -> Program -> Maybe Int
checkOverideClass cls prog = findMethodOverideConflict $ map (filter filterMethod . getFeatureForClass) (getAllInheritance cls prog)

checkForOverrideConflict :: Program -> (Maybe Int, String)
checkForOverrideConflict prog = let line = foldl (\acc element -> case acc of Nothing -> checkOverideClass element $ addBuiltInClass prog
                                                                              Just someline -> Just someline) Nothing prog in
                                (line, "overridden method conflict")
                                
-- sorting functions, these functions are useful for finding duplicates and for the printing later in the program

sortIdentifier :: Identifier -> Identifier -> Ordering
sortIdentifier (Identifier id1 no1) (Identifier id2 no2)
  | id1 > id2 = GT
  | id1 < id2 = LT
  | id1 == id2 = compare (read $ no1 :: Int) (read $ no2 :: Int)

sortClass :: Class -> Class -> Ordering
sortClass (Class id1 _ _ ) (Class id2 _ _) = sortIdentifier id1 id2

sortFeature :: Feature -> Feature -> Ordering
sortFeature (Attribute id1 _ _) (Attribute id2 _ _) = sortIdentifier id1 id2
sortFeature (Attribute _ _ _ ) (Method _ _ _ _) = LT
sortFeature (Method _ _ _ _) (Attribute _ _ _) = GT
sortFeature (Method id3 _ _ _) (Method id4 _ _ _) = sortIdentifier id3 id4

sortFormalById :: Formal -> Formal -> Ordering
sortFormalById (Formal x1 _) (Formal x2 _) = sortIdentifier x1 x2

sortCaseElement :: Case_element -> Case_element -> Ordering
sortCaseElement (Case_element _ x1 _) (Case_element _ x2 _) = sortIdentifier x1 x2

-- grouping functions, similar to sort functions which are helpful in finding duplicates

groupIdentifier :: Identifier -> Identifier -> Bool
groupIdentifier (Identifier id1 _) (Identifier id2 _) = id1 == id2

groupClass :: Class -> Class -> Bool
groupClass (Class id1 _ _) (Class id2 _ _) = groupIdentifier id1 id2

groupCaseElement :: Case_element -> Case_element -> Bool
groupCaseElement (Case_element _ x1 _) (Case_element _ x2 _) = groupIdentifier x1 x2

groupFeature :: Feature -> Feature -> Bool
groupFeature (Attribute id1 _ _) (Attribute id2 _ _) = groupIdentifier id1 id2
groupFeature (Attribute _ _ _) (Method _ _ _ _) = False
groupFeature (Method _ _ _ _) (Attribute _ _ _) = False
groupFeature (Method id3 _ _ _) (Method id4 _ _ _) = groupIdentifier id3 id4

-- utility functions, most of these functions are building blocks for the type-checking functions above

checkFormalTypeSelfClass :: Class -> Maybe Int
checkFormalTypeSelfClass (Class _ _ x) = case x of Nothing -> Nothing
                                                   Just feats -> checkFormalTypeSelfFeats feats

checkFormalTypeSelfFeats :: [Feature] -> Maybe Int
checkFormalTypeSelfFeats feats = foldl (\acc element -> case acc of Nothing -> checkFormalTypeSelfMethod element
                                                                    Just someline -> Just someline) Nothing $ filter filterMethod feats
                                       where checkFormalTypeSelfMethod (Method _ _ x _) = case x of Nothing -> Nothing
                                                                                                    Just forms -> foldl (\accf elementf -> case accf of Nothing -> case (getFormalTypeId elementf) of "SELF_TYPE" -> Just (getFormalTypeIdLineNumber elementf)
                                                                                                                                                                                                      _ -> Nothing
                                                                                                                                                        Just somelinef -> Just somelinef) Nothing forms

checkSelfInFormalClass :: Class -> Maybe Int
checkSelfInFormalClass (Class _ _ x) = case x of Nothing -> Nothing
                                                 Just feats -> checkSelfInFormalFeats feats

checkSelfInFormalFeats :: [Feature] -> Maybe Int
checkSelfInFormalFeats feats = foldl (\acc element -> case acc of Nothing -> checkSelfInFormalMethod element
                                                                  Just someline -> Just someline) Nothing $ filter filterMethod feats
                                     where checkSelfInFormalMethod (Method _ _ x _) = case x of Nothing -> Nothing
                                                                                                Just forms -> foldl (\accf elementf -> case accf of Nothing -> case (getFormalId elementf) of "self" -> Just (getFormalIdLineNumber elementf)
                                                                                                                                                                                              _ -> Nothing
                                                                                                                                                    Just somelinef -> Just somelinef) Nothing forms

checkDuplicateFormalClass :: Class -> Maybe Int
checkDuplicateFormalClass (Class _ _ x) = case x of Nothing -> Nothing
                                                    Just feats -> checkDuplicateFormalFeats feats

checkDuplicateFormalFeats :: [Feature] -> Maybe Int
checkDuplicateFormalFeats feats = foldl (\acc element -> case acc of Nothing -> checkDuplicateFormalMethod element
                                                                     Just someline -> Just someline) Nothing $ filter filterMethod feats
                                        where checkDuplicateFormalMethod (Method _ _ x _) = case x of Nothing -> Nothing
                                                                                                      Just forms -> checkFormsHelper $ List.sortBy sortFormalById  forms

checkFormsHelper :: [Formal] -> Maybe Int
checkFormsHelper (x:y:lst) = case (getFormalId x == getFormalId y) of True -> Just (getFormalIdLineNumber y)
                                                                      False -> checkFormsHelper (y:lst)
checkFormsHelper (x:[]) = Nothing
checkFormsHelper [] = Nothing

checkSelf :: Feature -> Maybe Int
checkSelf (Attribute x _ _) = case (getIdentifierName x) of "self" -> Just (getLineNumber x)
                                                            _ -> Nothing
checkSelf (Method _ _ _ _) = Nothing

-- filter functions that are used to filter methods/attributes from a list of Feature

filterNewClass :: Class -> Bool
filterNewClass cls = let cls_str = getClassName cls in
                     case cls_str of "Object" -> False
                                     "Int" -> False
                                     "String" -> False
                                     "IO" -> False
                                     "Bool" -> False
                                     _ -> True

filterAttribute :: Feature -> Bool
filterAttribute (Attribute _ _ _) = True
filterAttribute (Method _ _ _ _) = False

filterInitAttribute :: Feature -> Bool
filterInitAttribute (Method _ _ _ _) = False
filterInitAttribute (Attribute _ _ Nothing) = False
filterInitAttribute (Attribute _ _ (Just init_exp)) = True

filterMethod :: Feature -> Bool
filterMethod (Attribute _ _ _) = False
filterMethod (Method _ _ _ _) = True

findMethodOverideConflict :: [[Feature]] -> Maybe Int
findMethodOverideConflict (cls:checkwith) = foldl (\acc element -> case acc of Nothing -> getSource $ findFeatureContradictPair element checkwith
                                                                               Just someline -> Just someline) Nothing cls
findMethodOverideConflict [] = Nothing

findInheritanceAttributeDuplicate :: [[Feature]] -> Maybe Int
findInheritanceAttributeDuplicate (cls:checkwith) = foldl (\acc element -> case acc of Nothing -> getSource $ findFeatureContradictPair element checkwith
                                                                                       Just someline -> Just someline) Nothing cls
findInheritanceAttributeDuplicate [] = Nothing

-- otheer helper functions

getSource :: (Int, Int) -> Maybe Int
getSource (a, b)
  | a == b = Nothing
  | otherwise = Just b

findFeatureContradictPair :: Feature -> [[Feature]] -> (Int, Int)
findFeatureContradictPair feat feats = case feat of (Attribute _ _ _) -> foldl (\acc element -> findFeatureContraInList feat element acc) (getFeatureLineNumber feat, getFeatureLineNumber feat) feats
                                                    (Method _ _ _ _) -> foldl (\acc element -> case (fst acc == snd acc) of True -> findFeatureContraInList feat element acc
                                                                                                                            False -> acc) (getFeatureLineNumber feat, getFeatureLineNumber feat) feats

findFeatureContraInList :: Feature -> [Feature] -> (Int, Int) -> (Int, Int)
findFeatureContraInList feat feats orig_pair = foldl (\acc element -> case (compareFeature feat element) of True -> (getFeatureLineNumber element, fst acc)
                                                                                                            False -> acc) orig_pair feats

compareFeature :: Feature -> Feature -> Bool
compareFeature (Attribute x _ _) (Attribute y _ _) = (getIdentifierName x) == (getIdentifierName y)
compareFeature (Method x1 y1 z1 _) (Method x2 y2 z2 _) = case (getIdentifierName x1 == getIdentifierName x2) of False -> False
                                                                                                                True -> case (getIdentifierName y1 == getIdentifierName y2) of False -> True
                                                                                                                                                                               True -> not $ compareMaybeFormal z1 z2

compareFormal :: Formal -> Formal -> Bool
compareFormal (Formal _ x) (Formal _ y) = getIdentifierName x == getIdentifierName y

compareMaybeFormal :: Maybe [Formal] -> Maybe [Formal] -> Bool
compareMaybeFormal (Just forms1) (Just forms2) = case (List.length forms1 == List.length forms2) of True -> compareforms forms1 forms2
                                                                                                    False -> False
                                                      where compareforms [] [] = True
                                                            compareforms (hd1:tail1) (hd2:tail2) = case (compareFormal hd1 hd2) of True -> compareforms tail1 tail2
                                                                                                                                   False -> False
compareMaybeFormal (Just _) (Nothing) = False
compareMaybeFormal (Nothing) (Just _) = False
compareMaybeFormal (Nothing) (Nothing) = True

findDuplicateFeature :: [Feature] -> Maybe Int
findDuplicateFeature feats = let grouped = getLongSublist $ List.groupBy groupFeature $ List.sortBy sortFeature feats
                             in case (List.length grouped) of 0 -> Nothing
                                                              _ -> Just (getFeatureLineNumber ((List.sortBy sortFeature $ head grouped) !! 1))

getLongSublist :: [[a]] -> [[a]]
getLongSublist lst = filter (\each -> List.length each > 1) lst

-- some functions to extract information from the defined data types such as Program Class Feature etc

getFeatureLineNumber :: Feature -> Int
getFeatureLineNumber (Attribute x _ _) = getLineNumber x 
getFeatureLineNumber (Method x _ _ _) = getLineNumber x

checkClassInheritanceTree :: Class -> [String] -> Program -> Bool
checkClassInheritanceTree cls lst prog = case (getParent cls) of Nothing -> False
                                                                 Just parent -> case (getClass prog $ getIdentifierName parent) of Nothing -> False
                                                                                                                                   Just par_cls -> case (elem (getClassName par_cls) lst) of True -> True
                                                                                                                                                                                             False -> checkClassInheritanceTree par_cls ((getClassName par_cls):lst) prog
                                       
findAllValidTypes :: Program -> [String]
findAllValidTypes prog = foldl (\acc element -> acc ++ [getClassName element]) knownTypes prog

validmainMethod :: Feature -> Bool
validmainMethod feat = case feat of Attribute att_id type_id init_exp -> False
                                    Method met_id met_return met_formal _ -> case (getIdentifierName met_id) of "main" -> case met_formal of Nothing -> True
                                                                                                                                             Just forms -> False
                                                                                                                _ -> False

getIdentifierName :: Identifier -> String
getIdentifierName (Identifier x _) = x

getLineNumber :: Identifier -> Int
getLineNumber (Identifier _ x) = read x :: Int

getFormalId :: Formal -> String
getFormalId (Formal x _) = getIdentifierName x

getFormalIdLineNumber :: Formal -> Int
getFormalIdLineNumber (Formal x _) = getLineNumber x

getFormalTypeId :: Formal -> String
getFormalTypeId (Formal _ x) = getIdentifierName x

getFormalTypeIdLineNumber :: Formal -> Int
getFormalTypeIdLineNumber (Formal _ x) = getLineNumber x

getClassName :: Class -> String
getClassName (Class x _ _) = getIdentifierName x

getClassLineNumber :: Class -> Int
getClassLineNumber (Class x _ _) = getLineNumber x

getParent :: Class -> Maybe Identifier
getParent (Class _ x _) = x

getMethodNameId :: Feature -> String
getMethodNameId (Method x _ _ _) = getIdentifierName x

getMethodReturnType :: Feature -> String
getMethodReturnType (Method _ x _ _) = getIdentifierName x

getFeatureForClass :: Class -> [Feature]
getFeatureForClass (Class _ _ x) = case x of Nothing -> []
                                             Just feats -> feats

getAllInheritance :: Class -> Program -> [Class] -- This function does not check for inheritance cycle
getAllInheritance cls prog = let class_name = getClassName cls in
                             case class_name of "Object" -> [objectClass]
                                                "IO" -> [ioClass, objectClass]
                                                "String" -> [stringClass, objectClass]
                                                "Bool" -> [boolClass, objectClass]
                                                "Int" -> [intClass, objectClass]
                                                _ -> case (getParent cls) of Nothing -> [cls, objectClass]
                                                                             Just par -> case (getClass prog $ getIdentifierName par) of Nothing -> [cls, objectClass]
                                                                                                                                         Just parcls -> [cls] ++ getAllInheritance parcls prog

getAllFeaturesForClass :: Maybe Class -> Program -> [Feature] -- This function does not check for inheritance cycle
getAllFeaturesForClass cls prog = case cls of Nothing -> []
                                              Just clas -> foldl (\acc elem -> acc ++ (getFeatureForClass elem)) [] (getAllInheritance clas prog)

getFormalForMethod :: Feature -> [Formal]
getFormalForMethod (Method _ _ x _) = case x of Nothing -> []
                                                Just forms -> forms
getFormalForMethod (Attribute _ _ _) = []

getClass :: Program -> String -> Maybe Class
getClass prog name = List.find (\cls -> getClassName cls == name) prog

addBuiltInClass :: Program -> Program
addBuiltInClass prog = prog ++ [objectClass, ioClass, intClass, stringClass, boolClass]

getTypeFileName :: String -> String
getTypeFileName ast_file = take (List.length ast_file -4) ast_file ++ "-type"

flattenList :: [[a]] -> [a]
flattenList lst = foldl (\acc element -> acc ++ element) [] lst

getAllAttributeForClass :: Class -> Program -> [Feature]
getAllAttributeForClass cls prog = flattenList $ map ((filter filterAttribute) . getFeatureForClass) $ reverse $ getAllInheritance cls prog

getAllMethodForClass :: Class -> Program -> [Feature]
getAllMethodForClass cls prog = flattenList $ map ((filter filterMethod) . getFeatureForClass) $ reverse $ getAllInheritance cls prog

getMethodForClass :: Class -> [Feature]
getMethodForClass cls = filter filterMethod $ getFeatureForClass cls

-- Print Functions class map

printIdentifier :: Identifier -> [String]
printIdentifier id = (show $ getLineNumber id):([getIdentifierName id])

printProgram :: Program -> [String]
printProgram prog = foldl (\acc element -> acc ++ (printClass element prog)) ["class_map", show $ List.length prog] $ List.sortBy sortClass prog

printClass :: Class -> Program -> [String]
printClass cls prog = let all_attribute_for_class = getAllAttributeForClass cls prog
                      in foldl (\acc element -> acc ++ (printAttribute prog cls element)) [getClassName cls, show $ List.length all_attribute_for_class] all_attribute_for_class

printExpressions :: Program -> Map.Map String TypeCool -> Map.Map (String, String) [TypeCool] -> Class -> Maybe [Expression] -> [String]
printExpressions prog omap mmap cls exps = case exps of Just some_exps -> foldl (\acc element -> acc ++ printExpression prog omap mmap cls element) [show $ List.length some_exps] some_exps
                                                        Nothing -> ["0"]

printCaseElement :: Program -> Map.Map String TypeCool -> Map.Map (String, String) [TypeCool] -> Class -> Case_element -> [String]
printCaseElement prog omap mmap cls (Case_element case_var case_type_id case_element_body) = let bind_type_str = getIdentifierName case_type_id in
                                                                                             let bind_type = case bind_type_str of "SELF_TYPE" -> SelfType { type_id = getClassName cls }
                                                                                                                                   _ -> DirectType { type_id = bind_type_str } in
                                                                                             let updatedomap = Map.insert (getIdentifierName case_var) bind_type omap in
                                                                                             printIdentifier case_var ++ printIdentifier case_type_id ++ printExpression prog updatedomap mmap cls case_element_body

printLetBinding :: Program -> Map.Map String TypeCool -> Map.Map (String, String) [TypeCool] -> Class -> Let_binding -> [String]
printLetBinding prog omap mmap cls (Let_binding binding_var binding_type_id binding_init_exp) = case binding_init_exp of Nothing -> ["let_binding_no_init"] ++ printIdentifier binding_var ++ printIdentifier binding_type_id
                                                                                                                         Just some_init -> ["let_binding_init"] ++ printIdentifier binding_var ++ printIdentifier binding_type_id ++ printExpression prog omap mmap cls some_init

printAttribute :: Program -> Class -> Feature -> [String]
printAttribute prog cls (Attribute id_name type_name Nothing) = ["no_initializer", getIdentifierName id_name, getIdentifierName type_name]
printAttribute prog cls (Attribute id_name type_name (Just exp)) = let omap = buildClassMapForClass cls prog in
                                                                   let mmap = buildClassMethodMapForProgram prog in
                                                                   ["initializer", getIdentifierName id_name, getIdentifierName type_name] ++ printExpression prog omap mmap cls exp

printTypeExpression :: Program -> Map.Map String TypeCool -> Map.Map (String, String) [TypeCool] -> Class -> Expression -> String
printTypeExpression prog omap mmap cls exp = let output_type = findTypeForExpression prog omap mmap cls exp in
                                             case output_type of (DirectType x) -> x
                                                                 (SelfType y) -> "SELF_TYPE"

insertType :: [String] -> String -> [String]
insertType strs type_str = (head strs):type_str:(tail strs)

printExpression :: Program -> Map.Map String TypeCool -> Map.Map (String, String) [TypeCool] -> Class -> Expression -> [String]
printExpression prog omap mmap cls exp@(Assign exp_id var rhs) = insertType (printIdentifier exp_id) (printTypeExpression prog omap mmap cls exp) ++ printIdentifier var ++ printExpression prog omap mmap cls rhs
printExpression prog omap mmap cls exp@(Dynamic_dispatch exp_id e method args) = insertType (printIdentifier exp_id) (printTypeExpression prog omap mmap cls exp) ++ printExpression prog omap mmap cls e ++ printIdentifier method ++ printExpressions prog omap mmap cls args
printExpression prog omap mmap cls exp@(Static_dispatch exp_id e type_cast method args) = insertType (printIdentifier exp_id) (printTypeExpression prog omap mmap cls exp) ++ printExpression prog omap mmap cls e ++ printIdentifier type_cast ++ printIdentifier method ++ printExpressions prog omap mmap cls args
printExpression prog omap mmap cls exp@(Self_dispatch exp_id method args) = insertType (printIdentifier exp_id) (printTypeExpression prog omap mmap cls exp) ++ printIdentifier method ++ printExpressions prog omap mmap cls args
printExpression prog omap mmap cls exp@(If exp_id predicate then_exp else_exp) = insertType (printIdentifier exp_id) (printTypeExpression prog omap mmap cls exp) ++ printExpression prog omap mmap cls predicate ++ printExpression prog omap mmap cls then_exp ++ printExpression prog omap mmap cls else_exp
printExpression prog omap mmap cls exp@(While exp_id predicate while_body) = insertType (printIdentifier exp_id) (printTypeExpression prog omap mmap cls exp) ++ printExpression prog omap mmap cls predicate ++ printExpression prog omap mmap cls while_body
printExpression prog omap mmap cls exp@(Block exp_id block_body) = insertType (printIdentifier exp_id) (printTypeExpression prog omap mmap cls exp) ++ printExpressions prog omap mmap cls block_body
printExpression prog omap mmap cls exp@(New exp_id class_id) = insertType (printIdentifier exp_id) (printTypeExpression prog omap mmap cls exp) ++ printIdentifier class_id
printExpression prog omap mmap cls exp@(Isvoid exp_id e) = insertType (printIdentifier exp_id) (printTypeExpression prog omap mmap cls exp) ++ printExpression prog omap mmap cls e
printExpression prog omap mmap cls exp@(Plus exp_id x y) = insertType (printIdentifier exp_id) (printTypeExpression prog omap mmap cls exp) ++ printExpression prog omap mmap cls x ++ printExpression prog omap mmap cls y
printExpression prog omap mmap cls exp@(Minus exp_id x y) = insertType (printIdentifier exp_id) (printTypeExpression prog omap mmap cls exp) ++ printExpression prog omap mmap cls x ++ printExpression prog omap mmap cls y
printExpression prog omap mmap cls exp@(Times exp_id x y) = insertType (printIdentifier exp_id) (printTypeExpression prog omap mmap cls exp) ++ printExpression prog omap mmap cls x ++ printExpression prog omap mmap cls y
printExpression prog omap mmap cls exp@(Divide exp_id x y) = insertType (printIdentifier exp_id) (printTypeExpression prog omap mmap cls exp) ++ printExpression prog omap mmap cls x ++ printExpression prog omap mmap cls y
printExpression prog omap mmap cls exp@(Lt exp_id x y) = insertType (printIdentifier exp_id) (printTypeExpression prog omap mmap cls exp) ++ printExpression prog omap mmap cls x ++ printExpression prog omap mmap cls y
printExpression prog omap mmap cls exp@(Le exp_id x y) = insertType (printIdentifier exp_id) (printTypeExpression prog omap mmap cls exp) ++ printExpression prog omap mmap cls x ++ printExpression prog omap mmap cls y
printExpression prog omap mmap cls exp@(Eq exp_id x y) = insertType (printIdentifier exp_id) (printTypeExpression prog omap mmap cls exp) ++ printExpression prog omap mmap cls x ++ printExpression prog omap mmap cls y
printExpression prog omap mmap cls exp@(Not exp_id x) = insertType (printIdentifier exp_id) (printTypeExpression prog omap mmap cls exp) ++ printExpression prog omap mmap cls x
printExpression prog omap mmap cls exp@(Negate exp_id x) = insertType (printIdentifier exp_id) (printTypeExpression prog omap mmap cls exp) ++ printExpression prog omap mmap cls x
printExpression prog omap mmap cls exp@(Integer_exp exp_id int_cons) = insertType (printIdentifier exp_id) (printTypeExpression prog omap mmap cls exp) ++ [show int_cons]
printExpression prog omap mmap cls exp@(String_exp exp_id str_cons) = insertType (printIdentifier exp_id) (printTypeExpression prog omap mmap cls exp) ++ [str_cons]
printExpression prog omap mmap cls exp@(Identifier_exp exp_id id_exp) = insertType (printIdentifier exp_id) (printTypeExpression prog omap mmap cls exp) ++ printIdentifier id_exp
printExpression prog omap mmap cls exp@(Bool_exp exp_id bool_cons) = insertType (printIdentifier exp_id) (printTypeExpression prog omap mmap cls exp)
printExpression prog omap mmap cls exp@(Let_exp exp_id binding_list body_exp) = insertType (printIdentifier exp_id) (printTypeExpression prog omap mmap cls exp) ++ printBindings prog omap mmap cls binding_list ++ printExpressionWithBinding prog omap mmap cls body_exp binding_list
                                                                                where printBindings prog omap mmap cls lst = foldl (\acc element -> acc ++ printLetBinding prog omap mmap cls element) [show $ List.length lst] lst
                                                                                      printExpressionWithBinding prog omap mmap cls exp_body lst = let updatedomap =  updateMapWithLetBindings cls omap lst in
                                                                                                                                                   printExpression prog updatedomap mmap cls exp_body
printExpression prog omap mmap cls exp@(Case_exp exp_id case_body case_element_list) = insertType (printIdentifier exp_id) (printTypeExpression prog omap mmap cls exp) ++ printExpression prog omap mmap cls case_body ++ printCases prog omap mmap cls case_element_list
                                                                                       where printCases prog omap mmap cls lst = foldl (\acc element -> acc ++ printCaseElement prog omap mmap cls element) [show $ List.length lst] lst

-- print functions implementation map

printProgramImplementationMap :: Program -> [String]
printProgramImplementationMap prog = foldl (\acc element -> acc ++ (printClassImplementationMap element prog)) ["implementation_map", show $ List.length prog] $ List.sortBy sortClass prog

printClassImplementationMap :: Class -> Program -> [String]
printClassImplementationMap cls prog = let all_class_feature = getAllUniqueClassMethodTuples cls prog in
                                       [getClassName cls, show $ List.length all_class_feature] ++ printClassMethod prog cls all_class_feature
                                       where getAllUniqueClassMethodTuples cls prog = let all_classes = reverse $ getAllInheritance cls prog in
                                                                                      foldl (\acc element -> let method_for_class = getMethodForClass element in
                                                                                                             acc ++ foldl (\acc2 element2 -> let source_check = checkSourceMethod element2 element prog in
                                                                                                                                             case source_check of True -> acc2 ++ [getTupleWithStartEndClassFeat element2 prog element cls]
                                                                                                                                                                  False -> acc2) [] method_for_class) [] all_classes
                                                                                                                                                                  where getTupleWithStartEndClassFeat method prog endClass startClass = let inheritance_chain = reverse $ getAllInheritance startClass prog in
                                                                                                                                                                                                                                        foldl (\acc3 element3 -> case checkClassContainsMethod element3 method of False -> acc3
                                                                                                                                                                                                                                                                                                                  True -> (element3, getSameMethodClass element3 method)) (endClass, method) inheritance_chain

getSameMethodClass :: Class -> Feature -> Feature
getSameMethodClass cls (Method var_id _ _ _) = let methods = getMethodForClass cls in
                                               foldl (\acc element -> case getMethodNameId element == getIdentifierName var_id of True -> element
                                                                                                                                  False -> acc) (head methods) (tail methods)
                                                                                                                                                                                                                                                                                                               
checkClassContainsMethod :: Class -> Feature -> Bool
checkClassContainsMethod cls (Method name_id _ _ _) = let method_name = getIdentifierName name_id in
                                                      let all_method_names = map getMethodNameId (getMethodForClass cls) in
                                                      foldl (\acc element -> case acc of True -> True
                                                                                         False -> element == method_name) False all_method_names
  
checkSourceMethod :: Feature -> Class -> Program -> Bool
checkSourceMethod feat cls prog = let containsMethod = checkClassContainsMethod cls feat in
                                  case containsMethod of False -> False
                                                         True -> let all_parents = tail $ getAllInheritance cls prog in
                                                                 not $ foldl (\acc element -> case acc of True -> True
                                                                                                          False -> checkClassContainsMethod element feat) False all_parents

printClassMethod :: Program -> Class -> [(Class, Feature)] -> [String]
printClassMethod prog cls tuples = foldl (\acc element -> let forms = getFormalForMethod $ snd element in
                                                          acc ++ [getMethodNameId (snd element), show $ List.length forms] ++ (printFormals forms) ++ [getClassName $ fst element] ++ (printMethodBody prog (fst element) (snd element))) [] tuples
                                                          where printFormals fms = foldl (\acc2 element2 -> acc2 ++ [getFormalId element2]) [] fms

printMethodBody :: Program -> Class -> Feature -> [String]
printMethodBody prog cls (Method name_id return_type forms body_exp) = case body_exp of Nothing -> let clsmtd = case (getIdentifierName name_id) of "abort" -> "Object.abort"
                                                                                                                                                    "type_name" -> "Object.type_name"
                                                                                                                                                    "copy" -> "Object.copy"
                                                                                                                                                    "out_string" -> "IO.out_string"
                                                                                                                                                    "out_int" -> "IO.out_int"
                                                                                                                                                    "in_string" -> "IO.in_string"
                                                                                                                                                    "in_int" -> "IO.in_int"
                                                                                                                                                    "length" -> "String.length"
                                                                                                                                                    "concat" -> "String.concat"
                                                                                                                                                    "substr" -> "String.substr"
                                                                                                   in
                                                                                                   ["0", getIdentifierName return_type, "internal", clsmtd]
                                                                                        Just some_exp -> let omap = buildClassMapForClass cls prog in
                                                                                                         let updatedomap = updateMapWithMaybeFormal omap forms in
                                                                                                         let mmap = buildClassMethodMapForProgram prog in
                                                                                                         printExpression prog updatedomap mmap cls some_exp
                                                                                                         where updateMapWithMaybeFormal omap (Nothing) = omap
                                                                                                               updateMapWithMaybeFormal omap (Just some_forms) = foldl (\acc element -> Map.insert (getFormalId element) (DirectType { type_id = getFormalTypeId element}) acc) omap some_forms

-- print parent map

printParentMap :: Program -> [String]
printParentMap prog = let sorted_children = List.sortBy sortClass prog in
                      foldl (\acc element -> let class_name = getClassName element in
                                             case class_name of "Object" -> acc
                                                                _ -> let parent_class = getParent element in
                                                                     case parent_class of Nothing -> acc ++ [class_name, "Object"]
                                                                                          Just parent_str -> acc ++ [class_name, getIdentifierName parent_str]) ["parent_map", show $ (List.length prog) - 1] sorted_children
                      
-- print annotated ast, most of the printing is done by printExpression defined above

printAnnotatedAST :: Program -> [String]
printAnnotatedAST prog = let new_clses = List.filter filterNewClass prog in
                         foldl (\acc element -> acc ++ printAnnotatedASTClass element prog) [show $ List.length new_clses] new_clses

printAnnotatedASTClass :: Class -> Program -> [String]
printAnnotatedASTClass cls@(Class cls_id inherit_type feats) prog = case inherit_type of Nothing -> printIdentifier cls_id ++ ["no_inherits"] ++ printAnnotatedMaybeFeature prog cls feats
                                                                                         Just some_par -> printIdentifier cls_id ++ ["inherits"] ++ printIdentifier some_par ++ printAnnotatedMaybeFeature prog cls feats

printAnnotatedMaybeFeature :: Program -> Class -> Maybe [Feature] -> [String]
printAnnotatedMaybeFeature prog cls (Nothing) = ["0"]
printAnnotatedMaybeFeature prog cls (Just feats) = foldl (\acc element -> acc ++ printAnnotatedFeature prog cls element) [show $ List.length feats] feats

printAnnotatedFeature :: Program -> Class -> Feature -> [String]
printAnnotatedFeature prog cls feat@(Attribute name_id type_id init_exp) = let omap = buildClassMapForClass cls prog in
                                                                           let mmap = buildClassMethodMapForProgram prog in
                                                                           case init_exp of Nothing -> ["attribute_no_init"] ++ printIdentifier name_id ++ printIdentifier type_id
                                                                                            Just exp -> ["attribute_init"] ++ printIdentifier name_id ++ printIdentifier type_id ++ printExpression prog omap mmap cls exp
printAnnotatedFeature prog cls feat@(Method name_id type_id forms (Just body_exp)) = let omap = buildClassMapForClass cls prog in
                                                                                     let updatedomap = updateMapWithMaybeFormal omap forms in
                                                                                     let mmap = buildClassMethodMapForProgram prog in
                                                                                     ["method"] ++ printIdentifier name_id ++ printAnnotatedFormals forms ++ printIdentifier type_id ++ printExpression prog updatedomap mmap cls body_exp
                                                                                      where updateMapWithMaybeFormal omap (Nothing) = omap
                                                                                            updateMapWithMaybeFormal omap (Just some_forms) = foldl (\acc element -> Map.insert (getFormalId element) (DirectType { type_id = getFormalTypeId element}) acc) omap some_forms

printAnnotatedFormals :: Maybe [Formal] -> [String]
printAnnotatedFormals Nothing = ["0"]
printAnnotatedFormals (Just forms) = foldl (\acc element -> acc ++ printAnnotatedForm element) [show $ List.length forms] forms
                                     where printAnnotatedForm (Formal var_id type_id) = printIdentifier var_id ++ printIdentifier type_id

-- Expression Type Check

updateMapWithLetBindings :: Class -> Map.Map String TypeCool -> [Let_binding] -> Map.Map String TypeCool
updateMapWithLetBindings cls omap bindings = foldl (\acc element -> Map.insert (getBindingId element) (getBindingType element) acc) omap bindings
                                             where getBindingId (Let_binding x _ _) = getIdentifierName x
                                                   getBindingType (Let_binding _ y _) = let type_str = getIdentifierName y in
                                                                                        case type_str of "SELF_TYPE" -> SelfType { type_id = getClassName cls }
                                                                                                         _ -> DirectType { type_id = type_str }
                                             
convertStringToDirectType :: String -> TypeCool
convertStringToDirectType str = DirectType { type_id = str }

getExpressionForFeature :: Feature -> Expression
getExpressionForFeature (Attribute _ _ (Just init_exp)) = init_exp
getExpressionForFeature (Method _ _ _ (Just body_exp)) = body_exp

getAttributeIdAndTypes :: Class -> [Feature] -> [(String, TypeCool)]
getAttributeIdAndTypes cls feats = foldl (\acc element -> let type_str = getIdType element in
                                                          case type_str of "SELF_TYPE" -> acc ++ [(getIdName element, SelfType { type_id = getClassName cls })]
                                                                           _ -> acc ++ [(getIdName element, DirectType { type_id = getIdType element })]) [("self", SelfType { type_id = getClassName cls })] feats
                                     where getIdName (Attribute x y z) = getIdentifierName x
                                           getIdType (Attribute x y z) = getIdentifierName y

buildClassMapForClass :: Class -> Program -> Map.Map String TypeCool
buildClassMapForClass cls prog = let allattri = getAttributeIdAndTypes cls $ getAllAttributeForClass cls prog in
                                 Map.fromList allattri

getMethodIdAndFormalsAndReturn :: Program -> Class -> [((String, String), [TypeCool])]
getMethodIdAndFormalsAndReturn prog cls = let feats = getAllMethodForClass cls prog in
                                          foldl (\acc element -> let return_str = getMethodReturnType element in
                                                                 case return_str of "SELF_TYPE" -> acc ++ [((getClassName cls, getMethodNameId element), (map (convertStringToDirectType . getFormalTypeId) $ getFormalForMethod element) ++ [SelfType { type_id = getClassName cls }])]
                                                                                    _ -> acc ++ [((getClassName cls, getMethodNameId element), (map (convertStringToDirectType . getFormalTypeId) $ getFormalForMethod element) ++ [DirectType { type_id = return_str }])]) [] feats

buildClassMethodMapForProgram :: Program -> Map.Map (String, String) [TypeCool]
buildClassMethodMapForProgram prog = let allmethod = foldl (\acc element -> acc ++ (getMethodIdAndFormalsAndReturn prog element)) [] prog in
                                     Map.fromList allmethod

checkSubtype :: TypeCool -> TypeCool -> Program -> Bool
checkSubtype (DirectType type1) (DirectType type2) prog = let cls1 = getClass (addBuiltInClass prog) type1
                                                              cls2 = getClass (addBuiltInClass prog) type2
                                                          in
                                                          case (cls1, cls2) of (Just somecls1, Just somecls2) -> checkSubtypeClass somecls1 somecls2
                                                                               (_, _) -> False
                                                          where checkSubtypeClass x y = let listx = getAllInheritance x prog in
                                                                                        foldl (\acc element -> case acc of True -> True
                                                                                                                           False -> getClassName element == getClassName y) False listx
checkSubtype (SelfType type1) (SelfType type2) prog = type1 == type2
checkSubtype (SelfType type1) (DirectType type2) prog = checkSubtype (DirectType type1) (DirectType type2) prog
checkSubtype (DirectType type1) (SelfType type2) prog = False

findLeastUpperBound :: Program -> TypeCool -> TypeCool -> TypeCool
findLeastUpperBound prog (DirectType type1) (DirectType type2) = let cls1 = getClass (addBuiltInClass prog) type1
                                                                     cls2 = getClass (addBuiltInClass prog) type2
                                                                 in
                                                                 case (cls1, cls2) of (Just somecls1, Just somecls2) -> findFirstCommonParent somecls1 somecls2
                                                                                      (_, _) -> DirectType { type_id = "Object" }
                                                                 where findFirstCommonParent x y = let listx = getAllInheritance x prog
                                                                                                       listy = getAllInheritance y prog
                                                                                                   in
                                                                                                   DirectType { type_id = foldl (\acc element -> case acc of "Object" -> foldl (\acc2 element2 -> case acc2 of "Object" -> if getClassName element == getClassName element2 then getClassName element else acc2
                                                                                                                                                                                                               _ -> acc2) "Object" listy
                                                                                                                                                             _ -> acc) "Object" listx }
findLeastUpperBound prog (DirectType type1) (SelfType type2) = findLeastUpperBound prog (DirectType type1) (DirectType type2)
findLeastUpperBound prog (SelfType type1) (DirectType type2) = findLeastUpperBound prog (DirectType type1) (DirectType type2)
findLeastUpperBound prog (SelfType type1) (SelfType type2) = case type1 == type2 of True -> SelfType { type_id = type1 }
                                                                                    False -> DirectType { type_id = "Object" }
                                                                                    
getTypeStr :: TypeCool -> String
getTypeStr (DirectType x) = x
getTypeStr (SelfType x) = x

findTypeForExpression :: Program -> Map.Map String TypeCool -> Map.Map (String, String) [TypeCool] -> Class -> Expression -> TypeCool
findTypeForExpression prog omap mmap cls (Assign _ _ x) = findTypeForExpression prog omap mmap cls x
findTypeForExpression prog omap mmap cls (Dynamic_dispatch _ y x _) = let exp_type = findTypeForExpression prog omap mmap cls y in
                                                                      let return_type = case (Map.lookup (getTypeStr exp_type, getIdentifierName x) mmap) of Nothing -> DirectType { type_id = "not found" }
                                                                                                                                                             Just lst -> last lst in
                                                                      case return_type of (SelfType type1) -> exp_type
                                                                                          (DirectType type2) -> return_type
findTypeForExpression prog omap mmap cls (Static_dispatch _ y x z _) = let return_type = case (Map.lookup (getIdentifierName x, getIdentifierName z) mmap) of Nothing -> DirectType { type_id = "Object" }
                                                                                                                                                              Just lst -> last lst in
                                                                       case return_type of (SelfType type1) -> findTypeForExpression prog omap mmap cls y
                                                                                           (DirectType type2) -> return_type
findTypeForExpression prog omap mmap cls (Self_dispatch _ x _) = let return_type = case (Map.lookup (getClassName cls, getIdentifierName x) mmap) of Nothing -> DirectType { type_id = "Object" }
                                                                                                                                                     Just lst -> last lst in
                                                                 case return_type of (SelfType type1) -> SelfType { type_id = getClassName cls }
                                                                                     (DirectType type2) -> return_type
findTypeForExpression prog omap mmap cls (If _ _ x y) = let then_type = findTypeForExpression prog omap mmap cls x
                                                            else_type = findTypeForExpression prog omap mmap cls y
                                                        in
                                                        findLeastUpperBound prog then_type else_type
findTypeForExpression prog omap mmap cls (While _ x y) = DirectType { type_id = "Object" }
findTypeForExpression prog omap mmap cls (Block _ x) = case x of Nothing -> DirectType { type_id = "Object" }
                                                                 Just exp_lst -> findTypeForExpression prog omap mmap cls $ last exp_lst
findTypeForExpression prog omap mmap cls (New _ x) = case getIdentifierName x of "SELF_TYPE" -> SelfType { type_id = getClassName cls }
                                                                                 _ -> DirectType { type_id = getIdentifierName x }
findTypeForExpression prog omap mmap cls (Isvoid _ x) = DirectType { type_id = "Bool" }
findTypeForExpression prog omap mmap cls (Plus _ _ _) = DirectType { type_id = "Int" }
findTypeForExpression prog omap mmap cls (Minus _ _ _) = DirectType { type_id = "Int" }
findTypeForExpression prog omap mmap cls (Times _ _ _) = DirectType { type_id = "Int" }
findTypeForExpression prog omap mmap cls (Divide _ _ _) = DirectType { type_id = "Int" }
findTypeForExpression prog omap mmap cls (Lt _ _ _) = DirectType { type_id = "Bool" }
findTypeForExpression prog omap mmap cls (Le _ _ _) = DirectType { type_id = "Bool" }
findTypeForExpression prog omap mmap cls (Eq _ _ _) = DirectType { type_id = "Bool" }
findTypeForExpression prog omap mmap cls (Not _ _) = DirectType { type_id = "Bool" }
findTypeForExpression prog omap mmap cls (Negate _ _) = DirectType { type_id = "Int" }
findTypeForExpression prog omap mmap cls (Integer_exp _ _) = DirectType { type_id = "Int" }
findTypeForExpression prog omap mmap cls (String_exp _ _) = DirectType { type_id = "String" }
findTypeForExpression prog omap mmap cls (Identifier_exp _ x) = let result = Map.lookup (getIdentifierName x) omap in
                                                                case result of Nothing -> DirectType { type_id = "Object" }
                                                                               Just type1 -> type1
findTypeForExpression prog omap mmap cls (Bool_exp _ _) = DirectType { type_id = "Bool" }
findTypeForExpression prog omap mmap cls (Let_exp _ x y) = let updatedomap = updateMapWithLetBindings cls omap x in
                                                           findTypeForExpression prog updatedomap mmap cls y
findTypeForExpression prog omap mmap cls (Case_exp _ x y) = let case_exp_lst = foldl (\acc element -> acc ++ [findTypeForExpression prog (updateMapWithCase omap element) mmap cls (findExpressionWithCase element)]) [] y in
                                                            foldl (\acc2 element2 -> findLeastUpperBound prog acc2 element2) (head case_exp_lst) (tail case_exp_lst)
                                                            where updateMapWithCase omap (Case_element x2 y2 _) = Map.insert (getIdentifierName x2) (DirectType { type_id = getIdentifierName y2 }) omap
                                                                  findExpressionWithCase (Case_element _ _ z2) = z2

-- helper functions ends here --

getTypeForExpressionInClass :: Class -> Program -> Expression -> TypeCool
getTypeForExpressionInClass cls prog exp = let omap = buildClassMapForClass cls prog in
                                           let mmap = buildClassMethodMapForProgram prog in
                                           findTypeForExpression prog omap mmap cls exp

getTypeForExpressionInClassWithForms :: Class -> Program -> Maybe [Formal] -> Expression -> TypeCool
getTypeForExpressionInClassWithForms cls prog (Nothing) exp = getTypeForExpressionInClass cls prog exp
getTypeForExpressionInClassWithForms cls prog (Just forms) exp = let omap = updateWithForms forms $ buildClassMapForClass cls prog in
                                                                 let mmap = buildClassMethodMapForProgram prog in
                                                                 findTypeForExpression prog omap mmap cls exp
                                                                 where updateWithForms lst oldmap = foldl (\acc element -> Map.insert (getFormalId element) (DirectType { type_id = getFormalTypeId element }) acc) oldmap lst

-- expression type check functions, expressions are taken from initialization of attritues or method body 

checkForAttributeConform :: Program -> (Maybe Int, String)
checkForAttributeConform prog = let line = foldl (\acc element -> case acc of Just someline -> Just someline
                                                                              Nothing -> checkForAttributeConformClass element (addBuiltInClass prog)) Nothing prog in
                                (line, "attribute initialization does not conform")

checkForAttributeConformClass :: Class -> Program -> Maybe Int
checkForAttributeConformClass cls prog = let attributes = getAllAttributeForClass cls prog in
                                         foldl (\acc element -> case acc of Nothing -> checkConformAttribute element
                                                                            Just someline -> Just someline) Nothing attributes
                                         where checkConformAttribute (Attribute z x y) = case y of Nothing -> Nothing
                                                                                                   Just exp -> let declared_type_str = getIdentifierName x in
                                                                                                               let declared_type = case declared_type_str of "SELF_TYPE" -> SelfType { type_id = getClassName cls }
                                                                                                                                                             _ -> DirectType { type_id = declared_type_str } in
                                                                                                               let init_type = getTypeForExpressionInClass cls prog exp in
                                                                                                               case (checkSubtype init_type declared_type prog) of True -> Nothing
                                                                                                                                                                   False -> Just (getLineNumber z)

checkForMethodConform :: Program -> (Maybe Int, String)
checkForMethodConform prog = let line = foldl (\acc element -> case acc of Just someline -> Just someline
                                                                           Nothing -> checkForMethodConformClass element (addBuiltInClass prog)) Nothing prog in
                             (line, "method body does not conform to return type")

checkForMethodConformClass :: Class -> Program -> Maybe Int
checkForMethodConformClass cls prog = let methods = getAllMethodForClass cls prog in
                                      foldl (\acc element -> case acc of Nothing -> checkConformMethod element
                                                                         Just someline -> Just someline) Nothing methods
                                      where checkConformMethod (Method name return_id forms body) = case body of Nothing -> Nothing
                                                                                                                 Just exp -> let declared_type_str = getIdentifierName return_id in
                                                                                                                             let declared_type = case declared_type_str of "SELF_TYPE" -> SelfType { type_id = getClassName cls }
                                                                                                                                                                           _ -> DirectType { type_id = declared_type_str } in
                                                                                                                             let return_type = getTypeForExpressionInClassWithForms cls prog forms exp in
                                                                                                                             case (checkSubtype return_type declared_type prog) of True -> Nothing
                                                                                                                                                                                   False -> Just (getLineNumber name)

checkForExpressionType :: Program -> (Maybe Int, String)
checkForExpressionType prog = let updatedProg = addBuiltInClass prog in
                              let line = foldl (\acc element -> case acc of Just someline -> Just someline
                                                                            Nothing -> checkForExpressionTypeClass element updatedProg) Nothing prog in
                              (line, "expression type error")

checkForExpressionTypeClass :: Class -> Program -> Maybe Int
checkForExpressionTypeClass cls prog = let methods = getAllMethodForClass cls prog in
                                       let attributes = List.filter filterInitAttribute $ getAllAttributeForClass cls prog in
                                       foldl (\acc element -> case acc of Nothing -> checkExpressionFeature cls prog element
                                                                          Just someline -> Just someline) Nothing (methods ++ attributes)

checkExpressionFeature :: Class -> Program -> Feature -> Maybe Int
checkExpressionFeature cls prog (Attribute _ _ (Just init_exp)) = let omap = buildClassMapForClass cls prog in
                                                                  let mmap = buildClassMethodMapForProgram prog in
                                                                  checkExpression omap mmap cls prog init_exp
checkExpressionFeature cls prog (Method _ _ forms (Just body_exp)) = let omap = case forms of Nothing -> buildClassMapForClass cls prog
                                                                                              Just someform -> updateWithForms someform $ buildClassMapForClass cls prog in
                                                                     let mmap = buildClassMethodMapForProgram prog in
                                                                     checkExpression omap mmap cls prog body_exp
                                                                     where updateWithForms lst oldmap = foldl (\acc element -> Map.insert (getFormalId element) (DirectType { type_id = getFormalTypeId element }) acc) oldmap lst
checkExpressionFeature cls prog (Method _ _ forms (Nothing)) = Nothing

checkArgumentConform :: Map.Map String TypeCool -> Map.Map (String, String) [TypeCool] -> Class -> Program -> String -> Identifier -> Maybe [Expression] -> Maybe Int
checkArgumentConform omap mmap cls prog exp_type method (Nothing) = let args_in_map_maybe =  Map.lookup (exp_type, getIdentifierName method) mmap in
                                                                    case args_in_map_maybe of Nothing -> Just (getLineNumber method)
                                                                                              Just args_in_map -> if length (init args_in_map) == 0 then Nothing else Just (getLineNumber method)
checkArgumentConform omap mmap cls prog exp_type method (Just arg_list) = let args_in_map_maybe = Map.lookup (exp_type, getIdentifierName method) mmap in
                                                                          case args_in_map_maybe of Nothing -> Just (getLineNumber method)
                                                                                                    Just args_in_map_result -> let args_in_map = init args_in_map_result in 
                                                                                                                               case length args_in_map == length arg_list of False -> Just (getLineNumber method)
                                                                                                                                                                             True -> case checkArgumentsList omap mmap cls prog arg_list args_in_map of True -> Nothing
                                                                                                                                                                                                                                                        False -> Just (getLineNumber method)
                                                                                                                                                                                     where checkArgumentsList omap mmap cls prog (hd:lst) (hd2:lst2) = let this_type = findTypeForExpression prog omap mmap cls hd in
                                                                                                                                                                                                                                                       case checkSubtype this_type hd2 prog of True -> checkArgumentsList omap mmap cls prog lst lst2
                                                                                                                                                                                                                                                                                               False -> False
                                                                                                                                                                                           checkArgumentsList omap mmap cls prog [] [] = True


checkMaybeExpressions :: Map.Map String TypeCool -> Map.Map (String, String) [TypeCool] -> Class -> Program -> Maybe [Expression] -> Maybe Int
checkMaybeExpressions omap mmap cls prog (Nothing) = Nothing
checkMaybeExpressions omap mmap cls prog (Just exp_lst) = foldl (\acc element -> case acc of Just someline -> Just someline
                                                                                             Nothing -> checkExpression omap mmap cls prog element) Nothing exp_lst

checkExpression :: Map.Map String TypeCool -> Map.Map (String, String) [TypeCool] -> Class -> Program -> Expression -> Maybe Int
checkExpression omap mmap cls prog (Assign z x y) = let var_str = getIdentifierName x in
                                                    case var_str of "self" -> Just (getLineNumber x)
                                                                    _ -> let var_type = Map.lookup var_str omap in
                                                                         case var_type of Nothing -> Just (getLineNumber x)
                                                                                          Just vartype -> let init_exp_check = checkExpression omap mmap cls prog y in
                                                                                                          case init_exp_check of Just someline -> Just someline
                                                                                                                                 Nothing -> let init_type = findTypeForExpression prog omap mmap cls y in
                                                                                                                                            let compare_result = checkSubtype init_type vartype prog in
                                                                                                                                            case compare_result of True -> Nothing
                                                                                                                                                                   False -> Just (getLineNumber z)
checkExpression omap mmap cls prog (Dynamic_dispatch exp_id e method args) = let exp_type_check = checkExpression omap mmap cls prog e in
                                                                             case exp_type_check of Just someline -> Just someline
                                                                                                    Nothing -> let maybeExpressionCheck = checkMaybeExpressions omap mmap cls prog args in
                                                                                                               case maybeExpressionCheck of Just someline -> Just someline
                                                                                                                                            Nothing -> let exp_type_str = getTypeStr $ findTypeForExpression prog omap mmap cls e in
                                                                                                                                                       checkArgumentConform omap mmap cls prog exp_type_str method args
checkExpression omap mmap cls prog (Static_dispatch exp_id e type_cast method args) = let exp_type_check = checkExpression omap mmap cls prog e in
                                                                                      case exp_type_check of Just someline -> Just someline
                                                                                                             Nothing -> let cast_type_str = getIdentifierName type_cast in
                                                                                                                        case cast_type_str of "SELF_TYPE" -> Just (getLineNumber type_cast)
                                                                                                                                              _ -> let casted_type = DirectType { type_id = cast_type_str } in
                                                                                                                                                   let exp_type = findTypeForExpression prog omap mmap cls e in
                                                                                                                                                   case checkSubtype exp_type casted_type prog of False -> Just (getLineNumber type_cast)
                                                                                                                                                                                                  True -> let maybeExpressionCheck = checkMaybeExpressions omap mmap cls prog args in    
                                                                                                                                                                                                          case maybeExpressionCheck of Just someline -> Just someline
                                                                                                                                                                                                                                       Nothing -> let exp_type_str = getIdentifierName type_cast in
                                                                                                                                                                                                                                                  checkArgumentConform omap mmap cls prog exp_type_str method args
checkExpression omap mmap cls prog (Self_dispatch exp_id method args) = let maybeExpressionCheck = checkMaybeExpressions omap mmap cls prog args in
                                                                        case maybeExpressionCheck of Just someline -> Just someline
                                                                                                     Nothing -> let exp_type_str = getClassName cls in
                                                                                                                checkArgumentConform omap mmap cls prog exp_type_str method args
checkExpression omap mmap cls prog (If exp_id predicate then_exp else_exp) = let pre_type = findTypeForExpression prog omap mmap cls predicate in
                                                                             case checkSubtype pre_type (DirectType { type_id = "Bool" }) prog of False -> Just (getLineNumber exp_id)
                                                                                                                                                  True -> case checkExpression omap mmap cls prog then_exp of Just someline -> Just someline
                                                                                                                                                                                                              Nothing -> checkExpression omap mmap cls prog else_exp
checkExpression omap mmap cls prog (While exp_id predicate while_body) = let pre_type = findTypeForExpression prog omap mmap cls predicate in
                                                                         case checkSubtype pre_type (DirectType { type_id = "Bool" }) prog of False -> Just (getLineNumber exp_id)
                                                                                                                                              True -> checkExpression omap mmap cls prog while_body
checkExpression omap mmap cls prog (Block exp_id block_body) = case block_body of Nothing -> Just (getLineNumber exp_id)
                                                                                  Just exps -> foldl (\acc element -> case acc of Just someline -> Just someline
                                                                                                                                  Nothing -> case (checkExpression omap mmap cls prog element) of Nothing -> Nothing
                                                                                                                                                                                                  Just someline -> Just (getLineNumber exp_id)) Nothing exps
checkExpression omap mmap cls prog (New exp_id class_id) = let all_types = findAllValidTypes prog in
                                                           case List.find (\a -> a == getIdentifierName class_id) all_types of Nothing -> Just (getLineNumber class_id)
                                                                                                                               Just some_index -> Nothing
checkExpression omap mmap cls prog (Isvoid exp_id e) = checkExpression omap mmap cls prog e
checkExpression omap mmap cls prog (Plus exp_id x y) = case checkExpression omap mmap cls prog x of Just someline -> Just someline
                                                                                                    Nothing -> case checkExpression omap mmap cls prog y of Just someline -> Just someline
                                                                                                                                                            Nothing ->  let x_int = checkExpressionInteger omap mmap cls prog x in
                                                                                                                                                                        let y_int = checkExpressionInteger omap mmap cls prog y in
                                                                                                                                                                        case (x_int, y_int) of (True, True) -> Nothing
                                                                                                                                                                                               (_, _) -> Just (getLineNumber exp_id)
checkExpression omap mmap cls prog (Minus exp_id x y) = case checkExpression omap mmap cls prog x of Just someline -> Just someline
                                                                                                     Nothing -> case checkExpression omap mmap cls prog y of Just someline -> Just someline
                                                                                                                                                             Nothing ->  let x_int = checkExpressionInteger omap mmap cls prog x in
                                                                                                                                                                         let y_int = checkExpressionInteger omap mmap cls prog y in
                                                                                                                                                                         case (x_int, y_int) of (True, True) -> Nothing
                                                                                                                                                                                                (_, _) -> Just (getLineNumber exp_id)
checkExpression omap mmap cls prog (Times exp_id x y) = case checkExpression omap mmap cls prog x of Just someline -> Just someline
                                                                                                     Nothing -> case checkExpression omap mmap cls prog y of Just someline -> Just someline
                                                                                                                                                             Nothing ->  let x_int = checkExpressionInteger omap mmap cls prog x in
                                                                                                                                                                         let y_int = checkExpressionInteger omap mmap cls prog y in
                                                                                                                                                                         case (x_int, y_int) of (True, True) -> Nothing
                                                                                                                                                                                                (_, _) -> Just (getLineNumber exp_id)
checkExpression omap mmap cls prog (Divide exp_id x y) = case checkExpression omap mmap cls prog x of Just someline -> Just someline
                                                                                                      Nothing -> case checkExpression omap mmap cls prog y of Just someline -> Just someline
                                                                                                                                                              Nothing ->  let x_int = checkExpressionInteger omap mmap cls prog x in
                                                                                                                                                                          let y_int = checkExpressionInteger omap mmap cls prog y in
                                                                                                                                                                          case (x_int, y_int) of (True, True) -> Nothing
                                                                                                                                                                                                 (_, _) -> Just (getLineNumber exp_id)
checkExpression omap mmap cls prog (Lt exp_id x y) = case checkExpression omap mmap cls prog x of Just someline -> Just someline
                                                                                                  Nothing -> case checkExpression omap mmap cls prog y of Just someline -> Just someline
                                                                                                                                                          Nothing -> let x_type = findTypeForExpression prog omap mmap cls x in
                                                                                                                                                                     let y_type = findTypeForExpression prog omap mmap cls y in
                                                                                                                                                                     case checkExpressionsCompare omap mmap cls prog x_type y_type of True -> Nothing
                                                                                                                                                                                                                                      False -> Just (getLineNumber exp_id)
checkExpression omap mmap cls prog (Le exp_id x y) = case checkExpression omap mmap cls prog x of Just someline -> Just someline
                                                                                                  Nothing -> case checkExpression omap mmap cls prog y of Just someline -> Just someline
                                                                                                                                                          Nothing -> let x_type = findTypeForExpression prog omap mmap cls x in
                                                                                                                                                                     let y_type = findTypeForExpression prog omap mmap cls y in
                                                                                                                                                                     case checkExpressionsCompare omap mmap cls prog x_type y_type of True -> Nothing
                                                                                                                                                                                                                                      False -> Just (getLineNumber exp_id)
checkExpression omap mmap cls prog (Eq exp_id x y) = case checkExpression omap mmap cls prog x of Just someline -> Just someline
                                                                                                  Nothing -> case checkExpression omap mmap cls prog y of Just someline -> Just someline
                                                                                                                                                          Nothing -> let x_type = findTypeForExpression prog omap mmap cls x in
                                                                                                                                                                     let y_type = findTypeForExpression prog omap mmap cls y in
                                                                                                                                                                     case checkExpressionsCompare omap mmap cls prog x_type y_type of True -> Nothing
                                                                                                                                                                                                                                      False -> Just (getLineNumber exp_id)
checkExpression omap mmap cls prog (Not exp_id x) = case checkExpression omap mmap cls prog x of Just someline -> Just someline
                                                                                                 Nothing -> let exp_type = findTypeForExpression prog omap mmap cls x in
                                                                                                            case checkSubtype exp_type (DirectType { type_id = "Bool" }) prog of True -> Nothing
                                                                                                                                                                                 False -> Just (getLineNumber exp_id)
checkExpression omap mmap cls prog (Negate exp_id x) = case checkExpression omap mmap cls prog x of Just someline -> Just someline
                                                                                                    Nothing -> case checkExpressionInteger omap mmap cls prog x of True -> Nothing
                                                                                                                                                                   False -> Just (getLineNumber exp_id)
checkExpression omap mmap cls prog (Integer_exp exp_id int_cons) = Nothing
checkExpression omap mmap cls prog (String_exp exp_id string_cons) = Nothing
checkExpression omap mmap cls prog (Bool_exp exp_id cons) = Nothing
checkExpression omap mmap cls prog (Identifier_exp exp_id id_exp) = let look_up_result = Map.lookup (getIdentifierName id_exp) omap in
                                                                    case look_up_result of Nothing -> Just (getLineNumber id_exp)
                                                                                           Just someType -> Nothing
checkExpression omap mmap cls prog (Let_exp exp_id bindings body_exp) = let self_binding_check = checkBindingSelf bindings in
                                                                        case self_binding_check of Just someline -> Just someline
                                                                                                   Nothing ->  let binding_check = foldl (\acc element -> case acc of True -> checkLetBindingExpression omap mmap cls prog element
                                                                                                                                                                      False -> False) True bindings in
                                                                                                               case binding_check of False -> Just (getLineNumber exp_id)
                                                                                                                                     True -> let updatedomap = updateMapWithLetBindings cls omap bindings in
                                                                                                                                             checkExpression updatedomap mmap cls prog body_exp
checkExpression omap mmap cls prog (Case_exp exp_id case_body case_element_list) = let self_binding_check = checkBindingSelfCase case_element_list in
                                                                                   case self_binding_check of Just someline -> Just someline
                                                                                                              Nothing -> case checkExpression omap mmap cls prog case_body of Just someline -> Just someline
                                                                                                                                                                              Nothing -> case checkCaseDuplicate case_element_list of Just someline -> Just someline
                                                                                                                                                                                                                                      Nothing -> foldl (\acc element -> case acc of Just someline -> Just someline
                                                                                                                                                                                                                                                                                    Nothing -> checkCaseExpression omap mmap cls prog element) Nothing case_element_list

checkBindingSelfCase :: [Case_element] -> Maybe Int
checkBindingSelfCase cases = foldl (\acc element -> case acc of Just someline -> Just someline
                                                                Nothing -> checkSingleBindingSelfCase element) Nothing cases
                                                    where checkSingleBindingSelfCase (Case_element var_id _ _) = case getIdentifierName var_id of "self" -> Just (getLineNumber var_id)
                                                                                                                                                  _ -> Nothing

checkBindingSelf :: [Let_binding] -> Maybe Int
checkBindingSelf binds = foldl (\acc element -> case acc of Just someline -> Just someline
                                                            Nothing -> checkSingleBindingSelf element) Nothing binds
                                                where checkSingleBindingSelf (Let_binding var_id _ _) = case getIdentifierName var_id of "self" -> Just (getLineNumber var_id)
                                                                                                                                         _ -> Nothing 

checkCaseExpression :: Map.Map String TypeCool -> Map.Map (String, String) [TypeCool] -> Class -> Program -> Case_element -> Maybe Int
checkCaseExpression omap mmap cls prog (Case_element var_id type_id case_body) = let type_str = getIdentifierName type_id in
                                                                                 case type_str of "SELF_TYPE" -> Just (getLineNumber type_id)
                                                                                                  _ -> let updatedomap = Map.insert (getIdentifierName var_id) (DirectType { type_id = getIdentifierName type_id }) omap in
                                                                                                       checkExpression updatedomap mmap cls prog case_body


checkCaseDuplicate :: [Case_element] -> Maybe Int
checkCaseDuplicate cases = let long_subCases = getLongSublist $ List.groupBy groupCaseElement $ List.sortBy sortCaseElement cases in
                           case length long_subCases of 0 -> Nothing
                                                        _ -> Just (getCaseTypeLineNumber ((head long_subCases) !! 1))
                                                        where getCaseTypeLineNumber (Case_element _ x _) = getLineNumber x

checkLetBindingExpression :: Map.Map String TypeCool -> Map.Map (String, String) [TypeCool] -> Class -> Program -> Let_binding -> Bool
checkLetBindingExpression omap mmap cls prog (Let_binding binding_var binding_type Nothing) = case getIdentifierName binding_type of "SELF_TYPE" -> True
                                                                                                                                     _ -> let all_types = findAllValidTypes prog in
                                                                                                                                          case List.find (\a -> a == getIdentifierName binding_type) all_types of Nothing -> False
                                                                                                                                                                                                                  Just some_index -> True
checkLetBindingExpression omap mmap cls prog (Let_binding binding_var binding_type (Just init_exp)) = case getIdentifierName binding_type of "SELF_TYPE" -> True
                                                                                                                                             _ -> let all_types = findAllValidTypes prog in
                                                                                                                                                  case List.find (\a -> a == getIdentifierName binding_type) all_types of Nothing -> False
                                                                                                                                                                                                                          Just some_index -> case checkExpression omap mmap cls prog init_exp of Just someline -> False
                                                                                                                                                                                                                                                                                                 Nothing -> let init_type = findTypeForExpression prog omap mmap cls init_exp in
                                                                                                                                                                                                                                                                                                            let declared_type = DirectType { type_id = getIdentifierName binding_type } in
                                                                                                                                                                                                                                                                                                            case checkSubtype init_type declared_type prog of True -> True
                                                                                                                                                                                                                                                                                                                                                              False -> False 

checkExpressionsCompare :: Map.Map String TypeCool -> Map.Map (String, String) [TypeCool] -> Class -> Program -> TypeCool -> TypeCool -> Bool
checkExpressionsCompare omap mmap cls prog type1 type2 = let type1_str = getTypeStr type1 in
                                                         let type2_str = getTypeStr type2 in
                                                         case (type1_str, type2_str) of ("Int", "Int") -> True
                                                                                        ("Int", _) -> False
                                                                                        ("Bool", "Bool") -> True
                                                                                        ("Bool", _) -> False
                                                                                        ("String", "String") -> True
                                                                                        ("String", _) -> False
                                                                                        (_, "Int") -> False
                                                                                        (_, "Bool") -> False
                                                                                        (_, "String") -> False
                                                                                        (_, _) -> True

checkExpressionInteger :: Map.Map String TypeCool -> Map.Map (String, String) [TypeCool] -> Class -> Program -> Expression -> Bool
checkExpressionInteger omap mmap cls prog exp = let this_type = findTypeForExpression prog omap mmap cls exp in
                                                checkSubtype this_type (DirectType { type_id = "Int" }) prog

-- lists of type checking functions

-- functions that do not need type-checking expressions
errorChecks = [checkForCyclicInheritance, checkForInheritReserved, checkForClassReserved, checkForInvalidTypeProgram, checkForUseOfSelf, checkForMainClass, checkForMainMethod, checkForClassRedef, checkForDuplicateFeature, checkForDuplicateFormal, checkForFormalUseOfSelf, checkForFormalTypeSelf, checkForInheritedAttributeDuplicate, checkForOverrideConflict]

-- functions that type-check expressions
expressionErrors = [checkForExpressionType, checkForAttributeConform, checkForMethodConform]

-- one function that glues everything together
                                                
checkForErrors :: Program -> (Maybe Int, String)
checkForErrors prog = foldl (\acc element -> case acc of (Nothing, y) -> element prog
                                                         (Just someline, x) -> (Just someline, x)) (Nothing, "") (errorChecks ++ expressionErrors)
-- IO

outputFunctions = [printProgram, printProgramImplementationMap, printParentMap, printAnnotatedAST]

outputProgram :: [(Program -> [String])] -> Program -> [String]
outputProgram funs prog = foldl (\acc element -> acc ++ (element prog)) [] funs

-- execution starts here

main = do
  args <- getArgs
  withFile (head args) ReadMode (\handle -> do
                  contents <- hGetContents handle
                  let prog = processProgram $ lines contents
                  case (checkForErrors prog) of (Nothing, x) -> withFile (getTypeFileName $ head args) WriteMode (\handle2 -> do mapM (hPutStrLn handle2) $ outputProgram outputFunctions $ addBuiltInClass prog)
                                                (Just line_no, y) -> mapM putStr $ ["ERROR: ", show line_no, ": Type-Check: ", y, "\n"])
