let xml = ./xml
in let attr =  
    \(name: Text) ->  
    \(value : Text) -> 
    { name = name
    , value = value 
    }
in let na = ([] : List ./Attr)
in
{ html = xml "html"
, xml  = xml
, body = xml "body"
, head = xml "head"
, nav  = xml "nav"
, h1   = xml "h1"
, h1_   = xml "h1" na
, h2   = xml "h2"
, h2_   = xml "h2" na
, h3   = xml "h3"
, h3_   = xml "h3" na
, h4   = xml "h4"
, h4_   = xml "h4" na
, div  = xml "div"
, link = xml "link"
, script = \(a: List ./Attr) -> xml "script" a ([] : List Text)

, meta = \(a: List ./Attr) -> xml "meta" a ([] : List Text)

, a = xml "a"
, ul = xml "ul"
, p = xml "p"
, p_ = xml "p" na
, hr_ = xml "hr" na ([] : List Text)

, na = ([] : List ./Attr)
, nc = ([] : List Text)

, attr = attr

, id = attr "id"
, class = attr "class"
, href = attr "href"
, rel = attr "rel"
, charset = attr "charset"
, name = attr "name"
, content = attr "content"
, role = attr "role"
, crossorigin = attr "crossorigin"
, src = attr "src"
, style = attr "style"
, integrity = attr "integrity"
}
