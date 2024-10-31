type person = { name : string; age : int; hobbies : string list }
type comparator = person -> person -> int
type condition =
  | True
  | False
  | Age of (int -> bool)
  | Name of (string -> bool)
  | Hobbies of (string list -> bool)
  | And of condition * condition
  | Or of condition * condition
  | Not of condition
  | If of condition * condition * condition

(* TODO: Implement functions below *)

type db = person list

let newDatabase = []

let insert person db = person :: db

let remove name db = List.filter (fun person -> person.name <> name) db

let sort comparator db = List.sort comparator db

let rec query condition db = match condition with
  | True -> db
  | False -> []
  | Age(agecondition) -> List.filter (fun person -> agecondition person.age) db
  | Name(namecondition) -> List.filter (fun person -> namecondition person.name) db
  | Hobbies(hobbycondition) -> List.filter (fun person -> hobbycondition person.hobbies) db
  | And(condition1, condition2) -> query condition1 (query condition2 db)
  | Or(condition1, condition2) -> List.sort_uniq compare (query condition1 db @ query condition2 db)
  | Not(condition) -> List.filter (fun person -> not(List.mem person (query condition db))) db
  | If(condition1, condition2, condition3) -> List.concat (List.map (fun person -> if query condition1 [person] <> [] then query condition2 [person] else query condition3 [person]) db) 

let queryBy condition db comparator = 
  let querylist = query condition db in
  sort comparator querylist

let rec update condition db change = match db with 
  | [] -> []
  | person :: rest ->
    if query condition [person] <> []
    then (change person) :: (update condition rest change)
    else person :: (update condition rest change)


let deleteAll condition db = List.filter (fun person -> query condition [person] = []) db

