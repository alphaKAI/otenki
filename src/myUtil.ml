let rec range i j =
  if i > j then [] else
    i :: (range (i + 1) j)

let (>>) f g x = g (f x)

let (%%) = Printf.sprintf

open Core_kernel
let read_lines_into_string = In_channel.read_lines >> List.fold ~init:"" ~f:(fun a b -> a ^ b ^ "\n")

let has_opt opts opt = opts land opt <> 0