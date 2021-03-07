
module Alist = Otfed.Alist
module ResultMonad = Otfed.ResultMonad
module D = Otfed.Decode
module V = Otfed.Value


let dpoffset = 50

let display_x_scheme _units_per_em x = x
let display_y_scheme units_per_em y = units_per_em - y


let path_string_of_contour units_per_em (qpath : V.quadratic_path) =
  let display_x = display_x_scheme units_per_em in
  let display_y = display_y_scheme units_per_em in
  let circle ~color x y =
    Printf.sprintf "<circle cx=\"%d\" cy=\"%d\" r=\"5\" fill=\"%s\" />"
      (display_x x)
      (display_y y)
      color
  in
  let ((x0, y0), qelems) = qpath in
  let curve0 = Printf.sprintf "M%d,%d" (display_x x0) (display_y y0) in
  let circ0 = circle ~color:"green" x0 y0 in
  let (curveacc, circacc) =
    qelems |> List.fold_left (fun (curveacc, circacc) qelem ->
      match qelem with
      | V.QuadraticLineTo((xto, yto)) ->
          let circ = circle ~color:"green" xto yto in
          let curve = Printf.sprintf "L%d,%d" (display_x xto) (display_y yto) in
          (Alist.extend curveacc curve, Alist.extend circacc circ)

      | V.QuadraticCurveTo((x1, y1), (xto, yto)) ->
          let circ1 = circle ~color:"orange" x1 y1 in
          let circto = circle ~color:"green" xto yto in
          let curve = Printf.sprintf "Q%d,%d %d,%d" (display_x x1) (display_y y1) (display_x xto) (display_y yto) in
          (Alist.extend curveacc curve, Alist.append circacc [circ1; circto])
    ) (Alist.extend Alist.empty curve0, Alist.extend Alist.empty circ0)
  in
  let curves = Alist.to_list curveacc in
  let circs = Alist.to_list circacc in
  (Printf.sprintf "<path d=\"%s Z\" fill=\"none\" stroke=\"red\" stroke-width=\"5\" />" (String.concat " " curves), String.concat "" circs)


let make_simple (ttfcontours : V.ttf_simple_glyph_description) (bbox : V.bounding_box) (units_per_em : int) =
  let open ResultMonad in
  let display_x = display_x_scheme units_per_em in
  let display_y = display_y_scheme units_per_em in
  ttfcontours |> mapM D.path_of_ttf_contour >>= fun qpaths ->
  let xmin = bbox.V.x_min in
  let ymin = bbox.V.y_min in
  let xmax = bbox.V.x_max in
  let ymax = bbox.V.y_max in

  let pcs = (qpaths |> List.map (path_string_of_contour units_per_em)) in
  let paths = List.map (fun (x, _) -> x) pcs in
  let circs = List.map (fun (_, y) -> y) pcs in
  let ss =
    List.concat [
      [
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>";
        "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">";
        Printf.sprintf "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"%d\" height=\"%d\" viewBox=\"%d %d %d %d\">"
          (units_per_em + 2 * dpoffset)
          (ymax - ymin + 2 * dpoffset)
          (display_x (0 - dpoffset))
          (display_y (ymax + dpoffset))
          (units_per_em + 2 * dpoffset)
          (ymax - ymin + 2 * dpoffset);
        Printf.sprintf "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" fill=\"none\" stroke=\"purple\" stroke-width=\"5\" />"
          (display_x 0)
          (display_y ymax)
          units_per_em
          (ymax - ymin);
        Printf.sprintf "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" fill=\"none\" stroke=\"blue\" stroke-width=\"5\" />"
          (display_x xmin)
          (display_y ymax)
          (xmax - xmin)
          (ymax - ymin);
      ];
      paths;
      circs;
      ["</svg>"];
    ]
  in
  return @@ String.concat "" ss


let make (descr : V.ttf_glyph_description) ~bbox:(bbox : V.bounding_box) ~units_per_em:(units_per_em : int) =
  let open ResultMonad in
  match descr with
  | V.TtfSimpleGlyph(simple) -> make_simple simple bbox units_per_em
  | V.TtfCompositeGlyph(_)   -> return ""
