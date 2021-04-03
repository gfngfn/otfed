
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
