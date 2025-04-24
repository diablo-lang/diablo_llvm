(* TODO: Consider tagging instead of global counter *)
module NameGen = struct
  let counter = ref 0

  let fresh base =
    let n = !counter in
    incr counter;
    base ^ string_of_int n

  let reset () = counter := 0
end
