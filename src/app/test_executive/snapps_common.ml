(* snapps_common.ml -- common code to Snapps_generated, Snapps_constructed *)

open Core_kernel

let compatible_updates ~(ledger_update : Mina_base.Party.Update.t)
    ~(requested_update : Mina_base.Party.Update.t) : bool =
  (* the "update" in the ledger is derived from the account

     if the requested update has `Set` for a field, we
     should see `Set` for the same value in the ledger update

     if the requested update has `Keep` for a field, any
     value in the ledger update is acceptable

     for the app state, we apply this principle element-wise
  *)
  let open Mina_base.Snapp_basic.Set_or_keep in
  let compat req_item ledg_item ~equal =
    match (req_item, ledg_item) with
    | Keep, _ ->
        true
    | Set v1, Set v2 ->
        equal v1 v2
    | Set _, Keep ->
        false
  in
  let app_states_compat =
    let fs_requested =
      Pickles_types.Vector.Vector_8.to_list requested_update.app_state
    in
    let fs_ledger =
      Pickles_types.Vector.Vector_8.to_list ledger_update.app_state
    in
    List.for_all2_exn fs_requested fs_ledger ~f:(fun req ledg ->
        compat req ledg ~equal:Pickles.Backend.Tick.Field.equal)
  in
  let delegates_compat =
    compat requested_update.delegate ledger_update.delegate
      ~equal:Signature_lib.Public_key.Compressed.equal
  in
  let verification_keys_compat =
    compat requested_update.verification_key ledger_update.verification_key
      ~equal:
        [%equal:
          ( Pickles.Side_loaded.Verification_key.t
          , Pickles.Backend.Tick.Field.t )
          With_hash.t]
  in
  let permissions_compat =
    compat requested_update.permissions ledger_update.permissions
      ~equal:Mina_base.Permissions.equal
  in
  let snapp_uris_compat =
    compat requested_update.snapp_uri ledger_update.snapp_uri
      ~equal:String.equal
  in
  let token_symbols_compat =
    compat requested_update.token_symbol ledger_update.token_symbol
      ~equal:String.equal
  in
  let timings_compat =
    compat requested_update.timing ledger_update.timing
      ~equal:Mina_base.Party.Update.Timing_info.equal
  in
  let voting_fors_compat =
    compat requested_update.voting_for ledger_update.voting_for
      ~equal:Mina_base.State_hash.equal
  in
  List.for_all
    [ app_states_compat
    ; delegates_compat
    ; verification_keys_compat
    ; permissions_compat
    ; snapp_uris_compat
    ; token_symbols_compat
    ; timings_compat
    ; voting_fors_compat
    ]
    ~f:Fn.id
