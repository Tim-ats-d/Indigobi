open Indigobi.Gemini.Gemtext

let url_parsing () =
  assert (
    parse "=> gemini://example.org/"
    = [ Link { url = "gemini://example.org/"; name = None } ]);
  assert (
    parse "=> gemini://example.org/ An example link"
    = [ Link { url = "gemini://example.org/"; name = Some "An example link" } ]);
  assert (
    parse "=> gemini://example.org/foo\tAnother example link at the same host"
    = [
        Link
          {
            url = "gemini://example.org/foo";
            name = Some "Another example link at the same host";
          };
      ]);
  assert (
    parse "=> foo/bar/baz.txt\tA relative link"
    = [ Link { url = "foo/bar/baz.txt"; name = Some "A relative link" } ]);
  assert (
    parse "=> \tgopher://example.org:70/1 A gopher link"
    = [
        Link { url = "gopher://example.org:70/1"; name = Some "A gopher link" };
      ])

let text =
  {|
# One
## Two
### Three
#### Four

=>
=> Nice link

>A quote

```py
# Print hello world

print("Hello world")
```
```
code
```
|}

let pass () =
  url_parsing ();
  assert (
    parse text
    = [
        Heading (`H1, "One");
        Heading (`H2, "Two");
        Heading (`H3, "Three");
        Text "#### Four";
        Text "";
        Text "=>";
        Link { url = "Nice"; name = Some "link" };
        Text "";
        Quote "A quote";
        Text "";
        Preformat
          {
            alt = Some "py";
            text = "# Print hello world\n\nprint(\"Hello world\")\n";
          };
        Preformat { alt = None; text = "code\n" };
      ])
