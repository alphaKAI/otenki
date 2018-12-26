open Core_kernel
open MyUtil

let (//=) a b = a := !a / b

let color_convert_from_hex_to_rgb hex_col_str =
  let hex_col_str = "0x" ^
                    if String.nget hex_col_str 0 = '#' then
                      String.slice hex_col_str 1 0
                    else
                      hex_col_str
  in
  let color_of_int = ref (int_of_string hex_col_str) in
  let b = !color_of_int % 256 in color_of_int //= 256;
  let g = !color_of_int % 256 in color_of_int //= 256;
  let r = !color_of_int in
  [r; g; b]

class color (col_str : string) =
  let col = color_convert_from_hex_to_rgb col_str in
  object
    val col = col
    method get_col = col
    method convert_to_hex = Printf.sprintf "#%02X%02X%02X" (List.nth_exn col 0) (List.nth_exn col 1) (List.nth_exn col 2)
  end

let style_RESET         = 1 lsl 0
let style_BOLD          = 1 lsl 1
let style_WEAKEN        = 1 lsl 2
let style_ITALIC        = 1 lsl 3
let style_UNDERSCORE    = 1 lsl 4
let style_SLOW_BLINK    = 1 lsl 5
let style_FAST_BLINK    = 1 lsl 6
let style_INVERT        = 1 lsl 7
let style_INVISIBLE     = 1 lsl 8
let style_STRIKETHROUGH = 1 lsl 9

let get_string_width =
  let open Unicode in east_asian_width

let rec str_repeat ?(ret="") ?(i=0) str t =
  if i < t then
    str_repeat ~ret:(ret ^ str) ~i:(i + 1) str t
  else
    ret

(*
  width文字分を半角スペースで埋めて右寄せにして返す
  str: 対象の文字列
  width: 半角基準の幅
*)
let rjust str width = (str_repeat " " (width - (get_string_width str))) ^ str

(*
  width文字分を半角スペースで埋めて左寄せにして返す
  str: 対象の文字列
  width: 半角基準の幅
*)
let ljust str width = str ^ (str_repeat " " (width - (get_string_width str)))

let center ?(ljust=true) str width =
  let tmp = width - get_string_width str in
  let (ll, lr) = (str_repeat " " (tmp / 2), str_repeat " " (tmp - tmp / 2)) in
  if ljust then
    ll ^ str ^ lr
  else
    lr ^ str ^ ll

let change_style ?(conky=false) style_opts out =
  if has_opt style_opts style_RESET then
    if conky then
      Printf.fprintf out "${color}${font}"
    else
      Printf.fprintf out "\x1b[0m";
  let cmd = ref "\x1b[" in
  [
    style_RESET;
    style_WEAKEN;
    style_ITALIC;
    style_UNDERSCORE;
    style_SLOW_BLINK;
    style_FAST_BLINK;
    style_INVERT;
    style_INVISIBLE;
    style_STRIKETHROUGH;
  ] |> List.iteri ~f:(fun i opt ->
      if has_opt style_opts opt then
        cmd := !cmd ^ (Printf.sprintf "%d;" i));
  let cmd = String.slice !cmd 0 (-1) in
  Printf.fprintf out "%sm" cmd

let change_color ?(weaken=false) ?(conky=false) col out =
  let color = col#get_col in
  let k = if weaken then 0.5 else 1.0 in
  let (r, g, b) = (List.nth_exn color 0 |> float_of_int, List.nth_exn color 1 |> float_of_int, List.nth_exn color 2 |> float_of_int) in
  let (r, g, b) = (r *. k |> int_of_float, g *. k |> int_of_float, b *. k |> int_of_float) in
  let cmd =
    if conky then
      Printf.sprintf "${color %s}" (col#convert_to_hex)
    else
      Printf.sprintf "\x1b[38;2;%d;%d;%dm" r g b
  in
  Printf.fprintf out "%s" cmd
