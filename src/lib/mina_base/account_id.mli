[%%import "/src/config.mlh"]

open Core_kernel
open Mina_base_import

[%%versioned:
module Stable : sig
  module V1 : sig
    type t [@@deriving sexp, equal, compare, hash, yojson]
  end
end]

val create : Public_key.Compressed.t -> Token_id.t -> t

val empty : t

val public_key : t -> Public_key.Compressed.t

val token_id : t -> Token_id.t

val to_input : t -> Snark_params.Tick.Field.t Random_oracle.Input.Chunked.t

val gen : t Quickcheck.Generator.t

include Comparable.S with type t := t

include Hashable.S_binable with type t := t

[%%ifdef consensus_mechanism]

type var

val typ : (var, t) Snark_params.Tick.Typ.t

val var_of_t : t -> var

module Checked : sig
  open Snark_params
  open Tick

  val create : Public_key.Compressed.var -> Token_id.var -> var

  val public_key : var -> Public_key.Compressed.var

  val token_id : var -> Token_id.var

  val to_input :
    var -> Snark_params.Tick.Field.Var.t Random_oracle.Input.Chunked.t

  val equal : var -> var -> (Boolean.var, _) Checked.t

  val if_ : Boolean.var -> then_:var -> else_:var -> (var, _) Checked.t
end

[%%endif]
