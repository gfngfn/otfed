
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
