
module Error = EncodeError

type 'a ok = ('a, Error.t) result

type table = {
  tag      : Value.Tag.t;
  contents : string;
}


let compare_table (table1 : table) (table2 : table) =
  Value.Tag.compare table1.tag table2.tag
