open PrintUtil
open Weather
open MyUtil
open Core

let show_WEATHER = 1 lsl 0
let show_TEMPERATURE = 1 lsl 1
let show_PROBABILITY_OF_RAIN = 1 lsl 2
let show_AMOUNT_OF_RAIN = 1 lsl 3
let show_HUMIDITY = 1 lsl 4
let show_WITHOUT_COLORS = 1 lsl 5
let show_ALL = show_WEATHER lor show_TEMPERATURE lor show_PROBABILITY_OF_RAIN lor show_AMOUNT_OF_RAIN lor show_HUMIDITY

type weatherForcast = {
  url: string;
  weathers: weather List.t;
  updated_time: Time.t;
  point_name: string;
}

let zone = Time.Zone.find_exn "tyo"
let time_to_string_as_jst = Time.to_string_abs_trimmed ~zone:zone

open Nethttp_client.Convenience

let () = Nettls_gnutls.init ()

let fetch_weatherForcast url =
  let html = http_get url in
  let open Soup in
  let soup = parse html in
  let point_info_and_updated_time_str = soup $ "section.section-wrap:nth-child(2) > h2:nth-child(1)" |> texts in
  let point_info = List.nth_exn point_info_and_updated_time_str 0
  and updated_time_str = List.nth_exn point_info_and_updated_time_str 1 in
  let point_name = (Re.Pcre.extract ~rex:(Re.Pcre.regexp "(.+)の天気") point_info).(1) in
  let updated_time =
    let (dd, hh, mm) =
      let dhm = Re.Pcre.extract ~rex:(Re.Pcre.regexp "(\\d+)日(\\d+):(\\d+)発表") updated_time_str in
      (dhm.(1) |> int_of_string, dhm.(2) |> int_of_string, dhm.(3) |> int_of_string) in
    let today = Date.today ~zone:zone in
    let (year, month) = (Date.year today, Date.month today |> Month.to_int) in
    let time_str = Printf.sprintf "%d-%d-%d %02d:%02d" year month dd hh mm in
    Time.of_string time_str in

  let weathers =
    ["today"; "tomorrow"; "dayaftertomorrow"]
    |> List.map ~f:(fun day ->
        let selector_base = "#forecast-point-3h-%s > tbody:nth-child(1)" %% day in
        let date =
          let date_texts = soup $ (selector_base ^ " > tr:nth-child(1) > td:nth-child(2) > div:nth-child(1) > p:nth-child(1)") |> texts in
          List.slice date_texts 0 (-1)
          |> String.concat in

        let weathers =
          range 2 9
          |> List.map ~f:(fun i ->
              let selector = selector_base ^ (" > tr:nth-child(4) > td:nth-child(%d)" %% i) in
              List.nth_exn (soup $ selector |> texts) 0) in

        let is_past =
          range 2 9
          |> List.map ~f:(fun i ->
              let selector = selector_base ^ (Printf.sprintf " > tr:nth-child(4) > td:nth-child(%d) > p:nth-child(2)" i) in
              soup $ selector |> classes
              |> List.find ~f:(fun x -> x = "past")
              |> function
              | Some _ -> true
              | None -> false
            ) in

        let temperatures =
          range 1 8
          |> List.map ~f:(fun i ->
              let selector = selector_base ^ (Printf.sprintf " > tr:nth-child(6) > td:nth-child(%d)" i) in
              List.nth_exn (soup $ selector |> texts) 0
              |> float_of_string) in

        let probability_of_rains =
          range 2 9
          |> List.map ~f:(fun i ->
              let selector = selector_base ^ (Printf.sprintf " > tr:nth-child(7) > td:nth-child(%d)" i) in
              List.nth_exn (soup $ selector |> texts) 0
              |> int_of_string) in

        let amount_of_rains =
          range 1 8
          |> List.map ~f:(fun i ->
              let selector = selector_base ^ (Printf.sprintf " > tr:nth-child(9) > td:nth-child(%d)" i) in
              List.nth_exn (soup $ selector |> texts) 0
              |> float_of_string) in

        let humidities =
          range 2 9
          |> List.map ~f:(fun i ->
              let selector = selector_base ^ (Printf.sprintf " > tr:nth-child(10) > td:nth-child(%d)" i) in
              List.nth_exn (soup $ selector |> texts) 0
              |> int_of_string) in

        {
          date = date;
          weathers = weathers;
          temperatures = temperatures;
          is_past = is_past;
          probability_of_rains = probability_of_rains;
          amount_of_rains = amount_of_rains;
          humidities = humidities;
        }) in
  {
    url = url;
    weathers = weathers;
    updated_time = updated_time;
    point_name = point_name;
  }

let print_weatherForcast ?(show_opts=show_ALL) ?(conky=false) ?(days=2) wf =
  let max_width = List.fold ~init:0 ~f:(fun a b -> Int.max a (get_string_width b.date)) wf.weathers + 6 in
  let max_unit_width = List.slice wf.weathers 0 days
                       |> List.fold ~init:0 ~f:(fun _ w ->
                           w.weathers
                           |> List.fold ~init:0 ~f:(fun x y -> Int.max x (get_string_width y))) in

  Printf.printf "%s\n" (str_repeat "-" (max_width + (max_unit_width + 1) * 8));
  let date = Time.to_date ~zone:zone wf.updated_time in
  Printf.printf "%sの天気 (%d月%d日 %s 発表)\n" wf.point_name (Date.month date |> Month.to_int) (Date.day date) (Time.format wf.updated_time "%H:%M" ~zone:zone);
  Printf.printf "%s" (str_repeat " " max_width);
  ["03時"; "06時"; "09時"; "12時"; "15時"; "18時"; "21時"; "24時"]
  |> List.iter ~f:(fun l -> Printf.printf "%s" (center l (max_unit_width + 1)));
  Printf.printf "\n";

  Printf.printf "%s\n" (str_repeat "=" (max_width + (max_unit_width + 1) * 8));
  List.slice wf.weathers 0 days
  |> List.iter ~f:(fun w ->
      let no_color = show_opts land show_WITHOUT_COLORS <> 0 in
      if show_opts land show_WEATHER <> 0 then
        print_weather w max_width ~unit_width:max_unit_width ~no_color ~conky;
      if show_opts land show_TEMPERATURE <> 0 then
        print_temperature w max_width ~unit_width:max_unit_width ~no_color ~conky;
      if show_opts land show_PROBABILITY_OF_RAIN <> 0 then
        print_probability_of_rain w max_width ~unit_width:max_unit_width ~conky;
      if show_opts land show_AMOUNT_OF_RAIN <> 0 then
        print_amount_of_rain w max_width ~unit_width:max_unit_width ~conky;
      if show_opts land show_HUMIDITY <> 0 then
        print_humidity w max_width ~unit_width:max_unit_width ~conky;
      Printf.printf "%s\n" (str_repeat "=" (max_width + (max_unit_width + 1) * 8)))


(*
let (>>) f g x = g (f x)
let read_lines_into_string = In_channel.read_lines >> List.fold ~init:"" ~f:(fun a b -> a ^ b ^ "\n")

type user = {
  name: string;
  age: int;
  handle: string;
} [@@deriving sexp, show]

let gen_user name age handle =
  {
    name = name;
    age = age;
    handle = handle;
  }

let (>>) f g x = g (f x)

let serialize = sexp_of_user >> Sexp.to_string

let deserialize = Sexp.of_string >> user_of_sexp

let serialized_test () =
  let user = gen_user "Akihiro Shoji" 21 "alphaKAI" in
  Printf.printf "user: %s\n" (show_user user);
  let serialized = serialize user in
  Out_channel.write_all "serialized" ~data:serialized;
  Printf.printf "ok\n"

let deserialize_test () =
  let serialized = read_lines_into_string "serialized" in
  let user = deserialize serialized in
  Printf.printf "user: %s\n" (show_user user)
*)

