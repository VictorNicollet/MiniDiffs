type diff_change = FromOld of int * int | FromNew of int
type diff = { new_text : string ; changes : diff_change list }

val diff : string -> string -> diff
val apply : diff -> string -> string
