
module Alist = Otfed.Alist
module ResultMonad = Otfed.ResultMonad
module D = Otfed.Decode
module V = Otfed.Value


type q_contour_element =
  | OnCurve   of int * int
  | Quadratic of int * int * int * int

type q_contour = q_contour_element list


let make_contours (descr : V.glyph_description) : q_contour list =
  match descr with
  | V.CompositeGlyph(_) ->
      []

  | V.SimpleGlyph(contours) ->
      contours |> List.map (fun contour ->
        let (xfirst, yfirst) =
          match contour with
          | (_, x, y) :: _ -> (x, y)
          | []             -> assert false
        in
        let rec aux acc = function
          | []                                                  -> Alist.to_list acc
          | (true, x, y) :: tail                                -> aux (Alist.extend acc @@ OnCurve(x, y)) tail
          | (false, x1, y1) :: (true, x, y) :: tail             -> aux (Alist.extend acc @@ Quadratic(x1, y1, x, y)) tail
          | (false, x1, y1) :: (((false, x2, y2) :: _) as tail) -> aux (Alist.extend acc @@ Quadratic(x1, y1, (x1 + x2) / 2, (y1 + y2) / 2)) tail
          | (false, x1, y1) :: []                               -> Alist.to_list (Alist.extend acc @@ Quadratic(x1, y1, xfirst, yfirst))
        in
        aux Alist.empty contour
      )


let dpoffset = 50
let display_x x = x
let display_y y = 1000 - y


let path_string_of_contour qcontour =
  let (_, curveacc, circacc) =
    qcontour |> List.fold_left (fun (is_first, curveacc, circacc) qelem ->
      match qelem with
      | OnCurve(xto, yto) ->
          let prefix = if is_first then "M" else "L" in
          let circ =
            Printf.sprintf "<circle cx=\"%d\" cy=\"%d\" r=\"5\" fill=\"green\" />"
              (display_x xto)
              (display_y yto)
          in
          let curve = Printf.sprintf "%s%d,%d" prefix (display_x xto) (display_y yto) in
          (false, Alist.extend curveacc curve, Alist.extend circacc circ)

      | Quadratic(x1, y1, xto, yto) ->
          let circ =
            Printf.sprintf "<circle cx=\"%d\" cy=\"%d\" r=\"5\" fill=\"orange\" /><circle cx=\"%d\" cy=\"%d\" r=\"5\" fill=\"green\" />"
              (display_x x1)
              (display_y y1)
              (display_x xto)
              (display_y yto)
          in
          let curve = Printf.sprintf "Q%d,%d %d,%d" (display_x x1) (display_y y1) (display_x xto) (display_y yto) in
          (is_first, Alist.extend curveacc curve, Alist.extend circacc circ)
    ) (true, Alist.empty, Alist.empty)
  in
  let curves = Alist.to_list curveacc in
  let circs = Alist.to_list circacc in
  (Printf.sprintf "<path d=\"%s Z\" fill=\"none\" stroke=\"red\" stroke-width=\"5\" />" (String.concat " " curves), String.concat "" circs)


let make (descr : V.glyph_description) ~bbox:(bbox : V.bounding_box) ~advance_width:(adv : int) =
  let qcontours = make_contours descr in
  let xmin = bbox.V.x_min in
  let ymin = bbox.V.y_min in
  let xmax = bbox.V.x_max in
  let ymax = bbox.V.y_max in

  let pcs = (qcontours |> List.map path_string_of_contour) in
  let paths = List.map (fun (x, _) -> x) pcs in
  let circs = List.map (fun (_, y) -> y) pcs in
  let ss =
    List.concat [
      [
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>";
        "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">";
        Printf.sprintf "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"1000\" height=\"1100\" viewBox=\"%d %d %d %d\">"
          (display_x (0 - dpoffset))
          (display_y (ymax + dpoffset))
          (adv + 2 * dpoffset)
          (ymax - ymin + 2 * dpoffset);
        Printf.sprintf "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" fill=\"gray\" stroke=\"purple\" stroke-width=\"5\" />"
          (display_x 0)
          (display_y ymax)
          adv
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
  String.concat "" ss

(* for test *)
(*
let () =
  initialize () ;
(*
  let testword = List.map Uchar.of_char ['O'; 'C'; 'a'; 'm'; 'l'] in
  let res = get_width_of_word "Hlv" testword in
    res |> List.iter (fun adv -> print_endline (string_of_int adv))
*)
  let dcdr = get_decoder "Hlv" in
  let (paths, _) = svg_of_uchar (100, 100) dcdr (Uchar.of_char 'R') in
    print_endline paths
*)
