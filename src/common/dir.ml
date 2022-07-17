let in_home = Filename.concat (Sys.getenv "HOME")

let cache_dir =
  in_home
  @@ Filename.concat
       [%system { darwin = "Library/Caches"; unix = ".cache" }]
       [%system { darwin = "Indigobi"; unix = "indigobi" }]

let config_dir =
  in_home
    [%system
      {
        darwin = "Library/Preferences/indigobi.plist";
        unix = ".config/indigobi";
      }]
