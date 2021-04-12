
module Error = EncodeError

type 'a ok = ('a, Error.t) result

type table = {
  tag      : Value.Tag.t;
  contents : string;
}
