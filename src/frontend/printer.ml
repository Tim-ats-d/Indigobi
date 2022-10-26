open Common

module type S = sig
  val gemini : Theme.t -> Gemtext.line -> Img.t
end

module Default : S = struct
  let gemini (theme : Theme.t) = function
    | Gemtext.Text t -> Img.string theme.text t
    | Link { url; name } ->
        let open Notty.Infix in
        Img.string Attr.empty "=> "
        <|> Img.string theme.link url
        <|> Option.fold name ~none:Img.empty ~some:(fun name ->
                Img.string Attr.empty " " <|> Img.string theme.link_name name)
    | Preformat { text; _ } -> Img.string Attr.(fg (gray 4)) text
    | Heading (`H1, head) -> Img.string theme.h1 head
    | Heading (`H2, head) -> Img.string theme.h2 head
    | Heading (`H3, head) -> Img.string theme.h3 head
    | ListItem item -> Img.string theme.item ("* " ^ item)
    | Quote quote -> Img.string theme.quote ("█ " ^ quote)
end
