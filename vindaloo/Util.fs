module Vindaloo.Util

let rec splitList list endIdx = 
    match list with
    | xs when endIdx = 0 -> Some ([], xs)
    | h :: tail ->
        match splitList tail (endIdx-1) with
        | Some (alist,elist) -> Some (h :: alist, elist)
        | x -> x
    | _ -> None

let OptionAnd tf1 tf2 f =
    match tf1 with
    | None -> None
    | Some x -> match tf2 with
        | None -> None
        | Some y -> f x y
