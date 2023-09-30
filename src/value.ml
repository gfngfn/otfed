
open Basic


module Tag : sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val of_wide_int : wint -> t
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val format_version_OTTO : t
  val format_version_true : t
  val format_version_1_0 : t
  val format_version_ttcf : t

  val table_cmap : t
  val table_head : t
  val table_hhea : t
  val table_hmtx : t
  val table_maxp : t
  val table_name : t
  val table_os2  : t
  val table_post : t

  val table_cvt  : t
  val table_fpgm : t
  val table_glyf : t
  val table_loca : t
  val table_prep : t

  val table_cff  : t
  val table_cff2 : t
  val table_vorg : t

  val table_dsig : t
  val table_kern : t
  val table_vhea : t
  val table_vmtx : t

  val table_base : t
  val table_gdef : t
  val table_gpos : t
  val table_gsub : t
  val table_jstf : t
  val table_math : t

end = struct
  type t = wint

  let equal = WideInt.equal

  let compare = WideInt.compare

  let of_wide_int n = n

  let to_string tag =
    let ui = tag in
    let open WideInt in
    let b0 = ui lsr 24 in
    let r0 = ui -% (b0 lsl 24) in
    let b1 = r0 lsr 16 in
    let r1 = r0 -% (b1 lsl 16) in
    let b2 = r1 lsr 8 in
    let b3 = r1 -% (b2 lsl 8) in
    Printf.sprintf "%c%c%c%c"
      (to_byte b0)
      (to_byte b1)
      (to_byte b2)
      (to_byte b3)

  let pp ppf tag =
    Format.fprintf ppf "%s" (to_string tag)

  let format_version_OTTO = !%% 0x4F54544FL
  let format_version_true = !%% 0x74727565L
  let format_version_1_0  = !%% 0x00010000L
  let format_version_ttcf = !%% 0x74746366L

  let table_cmap = !%% 0x636D6170L
  let table_head = !%% 0x68656164L
  let table_hhea = !%% 0x68686561L
  let table_hmtx = !%% 0x686D7478L
  let table_maxp = !%% 0x6D617870L
  let table_name = !%% 0x6E616D65L
  let table_os2  = !%% 0x4F532F32L
  let table_post = !%% 0x706F7374L

  let table_cvt  = !%% 0x63767420L
  let table_fpgm = !%% 0x6670676DL
  let table_glyf = !%% 0x676C7966L
  let table_loca = !%% 0x6C6F6361L
  let table_prep = !%% 0x70726570L

  let table_cff  = !%% 0x43464620L
  let table_cff2 = !%% 0x43464632L
  let table_vorg = !%% 0x564F5247L

  let table_dsig = !%% 0x44534947L
  let table_kern = !%% 0x6B65726EL
  let table_vhea = !%% 0x76686561L
  let table_vmtx = !%% 0x766D7478L

  let table_base = !%% 0x42415345L
  let table_gdef = !%% 0x47444546L
  let table_gpos = !%% 0x47504F53L
  let table_gsub = !%% 0x47535542L
  let table_jstf = !%% 0x4A535446L
  let table_math = !%% 0x4d415448L

end

type glyph_id = int
[@@deriving show { with_path = false }]

type timestamp = wint
[@@deriving show { with_path = false }]

type design_units = int
[@@deriving show { with_path = false }]

type x_coordinate = design_units
[@@deriving show { with_path = false }]

type y_coordinate = design_units
[@@deriving show { with_path = false }]

type point = x_coordinate * y_coordinate
[@@deriving show { with_path = false }]

type bounding_box = {
  x_min : x_coordinate;
  y_min : y_coordinate;
  x_max : x_coordinate;
  y_max : y_coordinate;
}
[@@deriving show { with_path = false }]

type linear_transform = {
  a : float;
  b : float;
  c : float;
  d : float;
}
[@@deriving show { with_path = false }]

type cubic_path_element =
  | CubicLineTo  of point
  | CubicCurveTo of point * point * point
[@@deriving show { with_path = false }]

type cubic_path = point * cubic_path_element list
[@@deriving show { with_path = false }]

type quadratic_path_element =
  | QuadraticLineTo  of point
  | QuadraticCurveTo of point * point
[@@deriving show { with_path = false }]

type quadratic_path = point * quadratic_path_element list
[@@deriving show { with_path = false }]


let cubic_path_of_quadratic_path ((v, qpath_elems) : quadratic_path) : cubic_path =
  let ( ~$ ) = float_of_int in
  let ( ~! ) = int_of_float in

  let (_, cpath_elem_acc) =
    qpath_elems |> List.fold_left (fun (v0, acc) qpath_elem ->
      match qpath_elem with
      | QuadraticLineTo(v1) ->
          (v1, Alist.extend acc (CubicLineTo(v1)))

      | QuadraticCurveTo(v1, v2) ->
          let (x0, y0) = v0 in
          let (x1, y1) = v1 in
          let (x2, y2) = v2 in
          let vA = (~! (~$ (x0 + x1 * 2) /. 3.), ~! (~$ (y0 + y1 * 2) /. 3.)) in
          let vB = (~! (~$ (x2 + x1 * 2) /. 3.), ~! (~$ (y2 + y1 * 2) /. 3.)) in
          (v2, Alist.extend acc (CubicCurveTo(vA, vB, v2)))
    ) (v, Alist.empty)
  in
  (v, Alist.to_list cpath_elem_acc)


let bezier_bounding_box ((x0, y0) : point) ((x1, y1) : point) ((x2, y2) : point) ((x3, y3) : point) =
  let ( ~$ ) = float_of_int in

  let bezier_point (t : float) (r0 : design_units) (r1 : design_units) (r2 : design_units) (r3 : design_units) =
    if t < 0. then
      r0
    else if 1. < t then
      r3
    else
      let c1 = ~$ (3 * (-r0 + r1)) in
      let c2 = ~$ (3 * (r0 - 2 * r1 + r2)) in
      let c3 = ~$ (-r0 + 3 * (r1 - r2) + r3) in
      int_of_float @@ (~$ r0) +. t *. (c1 +. t *. (c2 +. t *. c3))
  in

  let aux (r0 : design_units) (r1 : design_units) (r2 : design_units) (r3 : design_units) =
    let a = -r0 + 3 * (r1 - r2) + r3 in
    let b = 2 * (r0 - 2 * r1 + r2) in
    let c = -r0 + r1 in
    if a = 0 then
      [r0; r3]
    else
      let det = b * b - 4 * a * c in
      if det < 0 then
        [r0; r3]
      else
        let delta = sqrt (~$ det) in
        let t_plus  = (-. (~$ b) +. delta) /. (~$ (2 * a)) in
        let t_minus = (-. (~$ b) -. delta) /. (~$ (2 * a)) in
        [r0; r3; bezier_point t_plus r0 r1 r2 r3; bezier_point t_minus r0 r1 r2 r3]
  in

  let xs = aux x0 x1 x2 x3 in
  let x_max = xs |> List.fold_left Stdlib.max x0 in
  let x_min = xs |> List.fold_left Stdlib.min x0 in
  let ys = aux y0 y1 y2 y3 in
  let y_max = ys |> List.fold_left Stdlib.max y0 in
  let y_min = ys |> List.fold_left Stdlib.min y0 in
  {
    x_min;
    y_min;
    x_max;
    y_max;
  }


let unite_bounding_boxes (current : bounding_box) (new_one : bounding_box) : bounding_box =
  {
    x_min = Stdlib.min current.x_min new_one.x_min;
    y_min = Stdlib.min current.y_min new_one.y_min;
    x_max = Stdlib.max current.x_max new_one.x_max;
    y_max = Stdlib.max current.y_max new_one.y_max;
  }


let bounding_box_by_points ((x1, y1) : point) ((x2, y2) : point) : bounding_box =
  {
    x_min = Stdlib.min x1 x2;
    x_max = Stdlib.max x1 x2;
    y_min = Stdlib.min y1 y2;
    y_max = Stdlib.max y1 y2;
  }


let calculate_bounding_box_of_path ((v, path_elems) : cubic_path) : bounding_box =
  let bbox =
    let (x, y) = v in
    { x_min = x; y_min = y; x_max = x; y_max = y }
  in
  let (_, bbox) =
    path_elems |> List.fold_left (fun (v0, bbox) path_elem ->
      let (v_new, bbox_new) =
        match path_elem with
        | CubicLineTo(v1) ->
            (v1, bounding_box_by_points v0 v1)

        | CubicCurveTo(v1, v2, v3) ->
            (v3, bezier_bounding_box v0 v1 v2 v3)
      in
      (v_new, unite_bounding_boxes bbox bbox_new)
    ) (v, bbox)
  in
  bbox


let calculate_bounding_box_of_paths (paths : cubic_path list) : bounding_box option =
  paths |> List.fold_left (fun bbox_opt path ->
    let bbox_new = calculate_bounding_box_of_path path in
    match bbox_opt with
    | None       -> Some(bbox_new)
    | Some(bbox) -> Some(unite_bounding_boxes bbox bbox_new)
  ) None


type device = {
  start_size   : int;
  delta_values : int list;
}
[@@deriving show { with_path = false }]

type value_record = {
  x_placement  : design_units option;
  y_placement  : design_units option;
  x_advance    : design_units option;
  y_advance    : design_units option;
  x_pla_device : int option;
  y_pla_device : int option;
  x_adv_device : int option;
  y_adv_device : int option;
}

type anchor_adjustment =
  | NoAnchorAdjustment
  | AnchorPointAdjustment  of int
  | DeviceAnchorAdjustment of device * device

type anchor = design_units * design_units * anchor_adjustment

type mark_class = int

type entry_exit_record = {
  entry_anchor : anchor option;
  exit_anchor  : anchor option;
}

type mark_record = mark_class * anchor

type base_record = (anchor option) array
  (* Arrays of this type are indexed by `mark_class`.
     UNDOCUMENTED (in OpenType 1.8.3): BaseRecord tables sometimes contain NULL pointers. *)

type component_record = (anchor option) array
  (* Arrays of this type are indexed by `mark_class`. *)

type ligature_attach = component_record list

type mark2_record = anchor array
  (* Arrays of this type are indexed by `mark_class`. *)

type class_value = int
[@@deriving show { with_path = false }]

module Cmap = struct
  module Mapping : sig
    type t

    val empty : t

    val find : Uchar.t -> t -> glyph_id option

    val add_single : Uchar.t -> glyph_id -> t -> t

    val add_incremental_range : start:Uchar.t -> last:Uchar.t -> gid:glyph_id -> t -> t

    val add_constant_range : start:Uchar.t -> last:Uchar.t -> gid:glyph_id -> t -> t

    val fold : (Uchar.t -> glyph_id -> 'a -> 'a) -> t -> 'a -> 'a

    val number_of_entries : t -> int
  end = struct

    module MapImpl = Map.Make(Uchar)

    type t = glyph_id MapImpl.t


    let empty = MapImpl.empty


    let find =
      MapImpl.find_opt


    let add_single =
      MapImpl.add


    let uchar_opt_of_int (scalar : int) : Uchar.t option =
      if Uchar.is_valid scalar then
        Some(Uchar.of_int scalar)
      else
        None


    let add_incremental_range ~start:uch_start ~last:uch_last ~gid map =
      let start = Uchar.to_int uch_start in
      let last = Uchar.to_int uch_last in
      let rec aux map scalar gid =
        if scalar > last then
          map
        else
          let map =
            match uchar_opt_of_int scalar with
            | Some(uch) -> map |> add_single uch gid
            | None      -> map
          in
          aux map (scalar + 1) (gid + 1)
      in
      aux map start gid


    let add_constant_range ~start:uch_start ~last:uch_last ~gid map =
      let start = Uchar.to_int uch_start in
      let last = Uchar.to_int uch_last in
      let rec aux map scalar =
        if scalar > last then
          map
        else
          let map =
            match uchar_opt_of_int scalar with
            | Some(uch) -> map |> add_single uch gid
            | None      -> map
          in
          aux map (scalar + 1)
      in
      aux map start


    let fold =
      MapImpl.fold


    let number_of_entries =
      MapImpl.cardinal

  end

  type subtable_ids = {
    platform_id : int;
    encoding_id : int;
  }

  type subtable = {
    subtable_ids : subtable_ids;
    mapping      : Mapping.t;
  }


  (* TODO: take language IDs into account for some platforms *)
  let compare_subtables (subtable1 : subtable) (subtable2 : subtable) =
    let p = subtable2.subtable_ids.platform_id - subtable1.subtable_ids.platform_id in
    if p = 0 then
      subtable2.subtable_ids.encoding_id - subtable1.subtable_ids.encoding_id
    else
      p


  type t = subtable set


  let subtables cmap = cmap

end

module Head = struct
  type mac_style = {
    bold      : bool;
    italic    : bool;
    underline : bool;
    outline   : bool;
    shadow    : bool;
    condensed : bool;
    extended  : bool;
  }
  [@@deriving show { with_path = false }]

  type t = {
    font_revision   : wint;
    flags           : int;
    units_per_em    : int;
    created         : timestamp;
    modified        : timestamp;
    mac_style       : mac_style;
    lowest_rec_ppem : int;
  }
  [@@deriving show { with_path = false }]
end

module Hhea = struct
  type t = {
    ascender         : int;
    descender        : int;
    line_gap         : int;
    caret_slope_rise : int;
    caret_slope_run  : int;
    caret_offset     : int;
  }
  [@@deriving show { with_path = false }]
end

module Os2 = struct
  type width_class =
    | WidthUltraCondensed
    | WidthExtraCondensed
    | WidthCondensed
    | WidthSemiCondensed
    | WidthMedium
    | WidthSemiExpanded
    | WidthExpanded
    | WidthExtraExpanded
    | WidthUltraExpanded
  [@@deriving show { with_path = false }]

  type fs_type = {
    restricted_license_embedding : bool;
    preview_and_print_embedding  : bool;
    editable_embedding           : bool;
    no_subsetting                : bool;
    bitmap_embedding_only        : bool;
  }
  [@@deriving show { with_path = false }]

  type fs_selection = {
    italic           : bool;
    underscore       : bool;
    negative         : bool;
    outlined         : bool;
    strikeout        : bool;
    bold             : bool;
    regular          : bool;
    use_typo_metrics : bool;
    wws              : bool;
    oblique          : bool;
  }
  [@@deriving show { with_path = false }]

  type t = {
    us_weight_class             : int;  (* values from 1 to 1000. *)
    us_width_class              : width_class;
    fs_type                     : fs_type;
    y_subscript_x_size          : design_units;
    y_subscript_y_size          : design_units;
    y_subscript_x_offset        : design_units;
    y_subscript_y_offset        : design_units;
    y_superscript_x_size        : design_units;
    y_superscript_y_size        : design_units;
    y_superscript_x_offset      : design_units;
    y_superscript_y_offset      : design_units;
    y_strikeout_size            : design_units;
    y_strikeout_position        : design_units;
    s_family_class              : int;
    panose                      : string;  (* 10 bytes. *)
    ul_unicode_range1           : wint;
    ul_unicode_range2           : wint;
    ul_unicode_range3           : wint;
    ul_unicode_range4           : wint;
    ach_vend_id                 : string;  (* 4 bytes. *)
    fs_selection                : fs_selection;
    s_typo_ascender             : design_units;
    s_typo_descender            : design_units;
    s_typo_linegap              : design_units;
    us_win_ascent               : design_units;
    us_win_descent              : design_units;
    ul_code_page_range1         : wint option;
    ul_code_page_range2         : wint option;
    s_x_height                  : design_units option;
    s_cap_height                : design_units option;
    us_default_char             : Uchar.t option; [@printer (pp_option pp_uchar)]
    us_break_char               : Uchar.t option; [@printer (pp_option pp_uchar)]
    us_lower_optical_point_size : int option;
    us_upper_optical_point_size : int option;
  }
  [@@deriving show {with_path = false}]
end

module Post = struct
  module GlyphNameArray : sig
    type t

    val pp : Format.formatter -> t -> unit

    val show : t -> string

    val access : glyph_id -> t -> string option

    val of_list : string list -> t

    val to_list : t -> string list

    val length : t -> int
  end = struct
    type t = string array

    let pp ppf names =
      let pp_sep ppf () = Format.fprintf ppf ",@ " in
      Format.fprintf ppf "%a"
        (Format.pp_print_list ~pp_sep Format.pp_print_string)
        (Array.to_list names)


    let show names =
      Format.asprintf "%a" pp names


    let access gid names =
      try
        Some(names.(gid))
      with
      | _ -> None


    let of_list = Array.of_list

    let to_list = Array.to_list

    let length = Array.length
  end

  type value = {
    italic_angle        : int;
    underline_position  : int;
    underline_thickness : int;
    is_fixed_pitch      : bool;
    min_mem_type_42     : int;
    max_mem_type_42     : int;
    min_mem_type_1      : int;
    max_mem_type_1      : int;
  }
  [@@deriving show { with_path = false }]

  type t = {
    value       : value;
    glyph_names : GlyphNameArray.t option;
  }
  [@@deriving show { with_path = false }]
end

module Name = struct
  type name_record = {
    platform_id : int;
    encoding_id : int;
    language_id : int;
    name_id     : int;
    name        : string;
  }
  [@@deriving show { with_path = false }]

  type lang_tag = string
  [@@deriving show { with_path = false }]

  type t = {
    name_records : name_record list;
    lang_tags    : (lang_tag list) option;
  }
  [@@deriving show { with_path = false }]
end

module Vhea = struct
  type metrics =
    | Version1_0 of {
        ascent  : int;
        descent : int;
      }
    | Version1_1 of {
        vert_typo_ascender  : int;
        vert_typo_descender : int;
        vert_typo_line_gap  : int;
      }
  [@@deriving show { with_path = false }]

  type t = {
    metrics          : metrics;
    caret_slope_rise : int;
    caret_slope_run  : int;
    caret_offset     : int;
  }
  [@@deriving show { with_path = false }]
end

module Ttf = struct
  type contour_element = {
    on_curve : bool;
    point    : point;
  }
  [@@deriving show { with_path = false }]

  type contour = contour_element list
  [@@deriving show { with_path = false }]

  type composition =
    | Vector   of x_coordinate * y_coordinate
    | Matching of int * int
  [@@deriving show { with_path = false }]

  type instruction = string
  [@@deriving show { with_path = false }]

  type simple_glyph_description = contour list * instruction
  [@@deriving show { with_path = false }]

  type composite_glyph_component = {
    component_glyph_id        : glyph_id;
    composition               : composition;
    component_scale           : linear_transform option;
    round_xy_to_grid          : bool;
    use_my_metrics            : bool;
    unscaled_component_offset : bool;
  }
  [@@deriving show { with_path = false }]

  type composite_glyph_description = {
    composite_components  : composite_glyph_component list;
    composite_instruction : instruction option;
  }
  [@@deriving show { with_path = false }]

  type glyph_description =
    | SimpleGlyph    of simple_glyph_description
    | CompositeGlyph of composite_glyph_description
  [@@deriving show { with_path = false }]

  type glyph_info = {
    bounding_box : bounding_box;
    description  : glyph_description;
  }
  [@@deriving show { with_path = false }]
end

module Math = struct
  type math_value_record = int * device option
  [@@deriving show { with_path = false }]

  type math_constants = {
    script_percent_scale_down                     : int;
    script_script_percent_scale_down              : int;
    delimited_sub_formula_min_height              : int;
    display_operator_min_height                   : int;
    math_leading                                  : math_value_record;
    axis_height                                   : math_value_record;
    accent_base_height                            : math_value_record;
    flattened_accent_base_height                  : math_value_record;
    subscript_shift_down                          : math_value_record;
    subscript_top_max                             : math_value_record;
    subscript_baseline_drop_min                   : math_value_record;
    superscript_shift_up                          : math_value_record;
    superscript_shift_up_cramped                  : math_value_record;
    superscript_bottom_min                        : math_value_record;
    superscript_baseline_drop_max                 : math_value_record;
    sub_superscript_gap_min                       : math_value_record;
    superscript_bottom_max_with_subscript         : math_value_record;
    space_after_script                            : math_value_record;
    upper_limit_gap_min                           : math_value_record;
    upper_limit_baseline_rise_min                 : math_value_record;
    lower_limit_gap_min                           : math_value_record;
    lower_limit_baseline_drop_min                 : math_value_record;
    stack_top_shift_up                            : math_value_record;
    stack_top_display_style_shift_up              : math_value_record;
    stack_bottom_shift_down                       : math_value_record;
    stack_bottom_display_style_shift_down         : math_value_record;
    stack_gap_min                                 : math_value_record;
    stack_display_style_gap_min                   : math_value_record;
    stretch_stack_top_shift_up                    : math_value_record;
    stretch_stack_bottom_shift_down               : math_value_record;
    stretch_stack_gap_above_min                   : math_value_record;
    stretch_stack_gap_below_min                   : math_value_record;
    fraction_numerator_shift_up                   : math_value_record;
    fraction_numerator_display_style_shift_up     : math_value_record;
    fraction_denominator_shift_down               : math_value_record;
    fraction_denominator_display_style_shift_down : math_value_record;
    fraction_numerator_gap_min                    : math_value_record;
    fraction_num_display_style_gap_min            : math_value_record;
    fraction_rule_thickness                       : math_value_record;
    fraction_denominator_gap_min                  : math_value_record;
    fraction_denom_display_style_gap_min          : math_value_record;
    skewed_fraction_horizontal_gap                : math_value_record;
    skewed_fraction_vertical_gap                  : math_value_record;
    overbar_vertical_gap                          : math_value_record;
    overbar_rule_thickness                        : math_value_record;
    overbar_extra_ascender                        : math_value_record;
    underbar_vertical_gap                         : math_value_record;
    underbar_rule_thickness                       : math_value_record;
    underbar_extra_descender                      : math_value_record;
    radical_vertical_gap                          : math_value_record;
    radical_display_style_vertical_gap            : math_value_record;
    radical_rule_thickness                        : math_value_record;
    radical_extra_ascender                        : math_value_record;
    radical_kern_before_degree                    : math_value_record;
    radical_kern_after_degree                     : math_value_record;
    radical_degree_bottom_raise_percent           : int;
  }
  [@@deriving show { with_path = false }]

  type math_kern = math_value_record list * math_value_record list
  [@@deriving show { with_path = false }]

  type math_kern_info_record = {
    top_right_math_kern    : math_kern option;
    top_left_math_kern     : math_kern option;
    bottom_right_math_kern : math_kern option;
    bottom_left_math_kern  : math_kern option;
  }
  [@@deriving show { with_path = false }]

  type math_italics_correction = glyph_id * math_value_record
  [@@deriving show { with_path = false }]

  type math_glyph_info = {
    math_italics_correction    : math_italics_correction list;
    math_top_accent_attachment : (glyph_id * math_value_record) list;
    math_kern_info             : (glyph_id * math_kern_info_record) list;
  }
  [@@deriving show { with_path = false }]

  type glyph_part_record = {
    glyph_id_for_part      : glyph_id;
    start_connector_length : design_units;
    end_connector_length   : design_units;
    full_advance           : design_units;
    f_extender             : bool;
  }
  [@@deriving show { with_path = false }]

  type math_glyph_variant_record = {
    variant_glyph       : glyph_id;
    advance_measurement : design_units;
  }
  [@@deriving show { with_path = false }]

  type math_glyph_construction = {
    glyph_assembly                 : (math_value_record * glyph_part_record list) option;
    math_glyph_variant_record_list : math_glyph_variant_record list;
  }
  [@@deriving show { with_path = false }]

  type math_variants = {
    min_connector_overlap : design_units;
    vert_glyph_assoc      : (glyph_id * math_glyph_construction) list;
    horiz_glyph_assoc     : (glyph_id * math_glyph_construction) list;
  }
  [@@deriving show { with_path = false }]

  type t = {
    math_constants  : math_constants;
    math_glyph_info : math_glyph_info;
    math_variants   : math_variants;
  }
  [@@deriving show { with_path = false }]
end
