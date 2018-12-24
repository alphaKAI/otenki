open WeatherForecast
open Core

let tsukuba_url = "https://tenki.jp/forecast/3/11/4020/8220/3hours.html"

let fetch_and_print_wf ~url ~days ~conky () =
  fetch_weatherForcast url
  |> print_weatherForcast ~conky ~days

let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Command line weather forcast viewer"
    [%map_open
      let url = flag "url" (optional_with_default tsukuba_url string) ~doc: "表示したい地域に対応するtenki.jpの3時間天気予報のURL"
      and days = flag "days" (optional_with_default 2 int) ~doc: "表示したい日数1~3(デフォルトは2)"
      and conky = flag "conky" no_arg ~doc:"conkyで使いたい場合指定する" in
      fetch_and_print_wf ~url ~days ~conky
    ]

let _ = Command.run command