let in_home = Filename.concat @@ Sys.getenv "HOME"

let cache_dir =
  in_home
    [%system { darwin = "Library/Caches/Indigobi"; unix = ".cache/indigobi" }]

let history_path = Filename.concat cache_dir "history"
let known_hosts_path = Filename.concat cache_dir "known_hosts"

let config_dir =
  in_home
    [%system
      {
        darwin = "Library/Preferences/indigobi.plist";
        unix = ".config/indigobi";
      }]
