
open Basic


type loc_format =
  | ShortLocFormat
  | LongLocFormat


module Head = struct

  type derived = {
    xmin : int;
    ymin : int;
    xmax : int;
    ymax : int;
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
end


module Cff = struct
  module Maxp = struct
    type t = {
      num_glyphs : int;
    }
    [@@deriving show { with_path = false }]
  end
end
