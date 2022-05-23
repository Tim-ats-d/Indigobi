open Indigobi.Common.Urllib

let pass () =
  let _ =
    assert (
      encode "Hello, World! @ 1nd1g0b1 œœ"
      = "Hello%2C%20World%21%20%40%201nd1g0b1%20%C5%93%C5%93")
  in
  let _ =
    assert (
      to_string
        {
          scheme = "gemini";
          domain = "gemini.circumlunar.space";
          port = 1965;
          path = "/news";
          query = "hello";
        }
      = "gemini://gemini.circumlunar.space/news?hello")
  in
  let _ =
    assert (
      to_string
        {
          scheme = "gemini";
          domain = "gemini.circumlunar.space";
          port = 1965;
          path = "/news";
          query = "";
        }
      = "gemini://gemini.circumlunar.space/news")
  in
  let _ =
    assert (
      parse "gemini://gemini.circumlunar.space/news?hello"
        "gemini.circumlunar.space"
      = {
          scheme = "gemini";
          domain = "gemini.circumlunar.space";
          port = 1965;
          path = "/news";
          query = "hello";
        })
  in
  let _ =
    assert (
      parse "https://www.fsf.org:443" "www.fsf.org"
      = {
          scheme = "https";
          domain = "www.fsf.org";
          port = 443;
          path = "/";
          query = "";
        })
  in
  let _ =
    assert (
      parse "/news?hello" "gemini.circumlunar.space"
      = {
          scheme = "gemini";
          domain = "gemini.circumlunar.space";
          port = 1965;
          path = "/news";
          query = "hello";
        })
  in
  let _ =
    assert (
      parse "geminispace.info/search?gemini" "geminispace.info"
      = {
          scheme = "gemini";
          domain = "geminispace.info";
          port = 1965;
          path = "/search";
          query = "gemini";
        })
  in
  let _ =
    assert (
      parse "tructruc.gmi" "geminispace.info"
      = {
          scheme = "gemini";
          domain = "geminispace.info";
          port = 1965;
          path = "/tructruc.gmi";
          query = "";
        })
  in
  let _ =
    assert (
      parse "tructruc.xyz" ""
      = {
          scheme = "gemini";
          domain = "tructruc.xyz";
          port = 1965;
          path = "/";
          query = "";
        })
  in
  ()
