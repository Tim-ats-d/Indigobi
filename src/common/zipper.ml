type 'a t = { past : 'a list; present : 'a; future : 'a list }

let make present = { past = []; present; future = [] }

let backward t =
  match t.past with
  | [] -> None
  | x :: xs -> Some { past = xs; present = x; future = t.present :: t.future }

let forward t =
  match t.future with
  | [] -> None
  | x :: xs -> Some { past = t.present :: t.future; present = x; future = xs }

let push t present = { past = t.present :: t.past; present; future = [] }
