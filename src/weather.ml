open Core_kernel
open PrintUtil

module SMap = Map.Make(String)

let color_map =
  let color_map = SMap.empty in
  let color_map = SMap.add_exn color_map ~key:"晴れ" ~data:(new color "#FFC235") in
  let color_map = SMap.add_exn color_map ~key:"曇り" ~data:(new color "#BBBBBB") in
  let color_map = SMap.add_exn color_map ~key:"小雨" ~data:(new color "#80C7E6") in
  let color_map = SMap.add_exn color_map ~key:"弱雨" ~data:(new color "#77ABEA") in
  let color_map = SMap.add_exn color_map ~key:"雨"   ~data:(new color "#437CE6") in
  color_map

let color_UNK   = new color "#FF0A70"
and color_RED   = new color "#F06060"
and color_BLUE  = new color "#17ABEB"
and color_WHITE = new color "#FFFFFF"


type weather = {
  date : string;
  weathers: string List.t;
  temperatures: float List.t;
  is_past: bool List.t;
  probability_of_rains: int List.t;
  amount_of_rains: float List.t;
  humidities: int List.t;
} [@@deriving sexp]

exception PrintError

let print_weather ?(unit_width=4) ?(no_color=false) ?(conky=false) weather width =
  Printf.printf "%s" (rjust ("[" ^ weather.date ^"] | ") width);
  List.zip weather.weathers weather.is_past
  |> function
  | None -> raise PrintError
  | Some zipped -> zipped
                   |> List.iter ~f:(fun (weather, past) ->
                       let style = ref style_BOLD in
                       if past then
                         style := !style lor style_WEAKEN;
                       if not no_color then
                         begin
                           let color =
                             match SMap.find color_map weather with
                             | Some color -> color
                             | None -> color_UNK in
                           change_color color stdout ~weaken:past ~conky
                         end;
                       change_style !style stdout ~conky;
                       Printf.printf "%s" (rjust weather unit_width);
                       change_style style_RESET stdout ~conky;
                       Printf.printf " ");
    Printf.printf "\n"

exception NoMaxElement
exception NoMinElement

let print_temperature ?(unit_width=4) ?(no_color=false) ?(conky=false) weather width =
  let max_temperature = List.max_elt weather.temperatures ~compare:Float.compare
                        |> function
                        | None -> raise NoMaxElement
                        | Some max_elem -> max_elem
  and min_temperature = List.min_elt weather.temperatures ~compare:Float.compare
                        |> function
                        | None -> raise NoMinElement
                        | Some min_elem -> min_elem in
  Printf.printf "%s" (rjust "気温(度) | " width);
  List.zip weather.temperatures weather.is_past
  |> function
  | None -> raise PrintError
  | Some zipped -> zipped
                   |> List.iter ~f:(fun (temp, past) ->
                       change_color color_WHITE stdout ~weaken:past ~conky;
                       if past then
                         change_style style_WEAKEN stdout ~conky;
                       if not no_color then
                         if temp = max_temperature then
                           begin
                             change_style style_BOLD stdout ~conky;
                             change_color color_RED stdout ~weaken:past ~conky
                           end
                         else if temp = min_temperature then
                           begin
                             change_style style_BOLD stdout ~conky;
                             change_color color_BLUE stdout ~weaken:past ~conky
                           end;

                       Printf.printf "%s" (rjust (Printf.sprintf "%.1f" temp) unit_width);
                       change_style style_RESET stdout ~conky;
                       Printf.printf " ");
    Printf.printf "\n"

let print_probability_of_rain ?(unit_width=4) ?(conky=false) weather width =
  Printf.printf "%s" (rjust ("降水確率(%) | ") width);
  List.zip weather.probability_of_rains weather.is_past
  |> function
  | None -> raise PrintError
  | Some zipped -> zipped
                   |> List.iter ~f:(fun (prb_rain, past) ->
                       change_color color_WHITE stdout ~weaken:past ~conky;
                       Printf.printf "%s" (rjust (Printf.sprintf "%d" prb_rain) unit_width);
                       change_style style_RESET stdout ~conky;
                       Printf.printf " ");
    Printf.printf "\n"

let print_amount_of_rain ?(unit_width=4) ?(conky=false) weather width =
  Printf.printf "%s" (rjust ("降水量(mm/h) | ") width);
  List.zip weather.amount_of_rains weather.is_past
  |> function
  | None -> raise PrintError
  | Some zipped -> zipped
                   |> List.iter ~f:(fun (amo_rain, past) ->
                       change_color color_WHITE stdout ~weaken:past ~conky;
                       Printf.printf "%s" (rjust (Printf.sprintf "%.1f" amo_rain) unit_width);
                       change_style style_RESET stdout ~conky;
                       Printf.printf " ");
    Printf.printf "\n"

let print_humidity ?(unit_width=4) ?(conky=false) weather width =
  Printf.printf "%s" (rjust ("湿度(%) | ") width);
  List.zip weather.humidities weather.is_past
  |> function
  | None -> raise PrintError
  | Some zipped -> zipped
                   |> List.iter ~f:(fun (humid, past) ->
                       change_color color_WHITE stdout ~weaken:past ~conky;
                       Printf.printf "%s" (rjust (Printf.sprintf "%d" humid) unit_width);
                       change_style style_RESET stdout ~conky;
                       Printf.printf " ");
    Printf.printf "\n"