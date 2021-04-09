
open Basic


type loc_format =
  | ShortLocFormat
  | LongLocFormat
[@@deriving show { with_path = false }]


module Head = struct

  type derived = {
    x_min               : int;
    y_min               : int;
    x_max               : int;
    y_max               : int;
    index_to_loc_format : loc_format;
  }
  [@@deriving show { with_path = false }]

  type t = {
    value   : Value.Head.t;
    derived : derived;
  }
  [@@deriving show { with_path = false }]

end


module Hhea = struct

  type derived = {
    advance_width_max      : int;
    min_left_side_bearing  : int;
    min_right_side_bearing : int;
    xmax_extent            : int;
  }
  [@@deriving show { with_path = false }]

  type t = {
    value   : Value.Hhea.t;
    derived : derived;
  }
  [@@deriving show { with_path = false }]

end


module Os2 = struct
  type derived = {
    x_avg_char_width    : int;
    ul_unicode_range1   : wint;
    ul_unicode_range2   : wint;
    ul_unicode_range3   : wint;
    ul_unicode_range4   : wint;
    us_first_char_index : int;
    us_last_char_index  : int;
    us_max_context      : int option;
  }
  [@@deriving show { with_path = false }]

  type t = {
    value   : Value.Os2.t;
    derived : derived;
  }
  [@@deriving show { with_path = false }]
end


module Ttf = struct
  module Maxp = struct
    type t = {
      num_glyphs               : int;
      max_points               : int;
      max_contours             : int;
      max_composite_points     : int;
      max_composite_contours   : int;
      max_zones                : int;
      max_twilight_points      : int;
      max_storage              : int;
      max_function_defs        : int;
      max_instruction_defs     : int;
      max_stack_elements       : int;
      max_size_of_instructions : int;
      max_component_elements   : int;
      max_component_depth      : int;
    }
    [@@deriving show { with_path = false }]
  end

  type flag = {
    on_curve       : bool;
    x_short_vector : bool;
    y_short_vector : bool;
    this_x_is_same : bool;
    this_y_is_same : bool;
  }

  type component_flag = {
    arg_1_and_2_are_words    : bool;
    args_are_xy_values       : bool;
    round_xy_to_grid         : bool;
    we_have_a_scale          : bool;
    we_have_an_x_and_y_scale : bool;
    we_have_a_two_by_two     : bool;
    we_have_instructions     : bool;
    use_my_metrics           : bool;
  }
end


module Cff = struct
  module Maxp = struct
    type t = {
      num_glyphs : int;
    }
    [@@deriving show { with_path = false }]
  end
end
