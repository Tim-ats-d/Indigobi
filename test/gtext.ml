open Indigobi.Gemini.Text

let pass () =
  let _ =
    assert (
      parse "=> gemini://example.org/"
      = [ Link { url = "gemini://example.org/"; name = None } ])
  in
  let _ =
    assert (
      parse "=> gemini://example.org/ An example link"
      = [
          Link { url = "gemini://example.org/"; name = Some "An example link" };
        ])
  in
  let _ =
    assert (
      parse "=> gemini://example.org/foo\tAnother example link at the same host"
      = [
          Link
            {
              url = "gemini://example.org/foo";
              name = Some "Another example link at the same host";
            };
        ])
  in
  let _ =
    assert (
      parse "=> foo/bar/baz.txt\tA relative link"
      = [ Link { url = "foo/bar/baz.txt"; name = Some "A relative link" } ])
  in
  let _ =
    assert (
      parse "=> \tgopher://example.org:70/1 A gopher link"
      = [
          Link
            { url = "gopher://example.org:70/1"; name = Some "A gopher link" };
        ])
  in
  ()
