open Cmt_format
open Typedtree

let ignored t _loc =
  Format.printf "#ignored: %s@;" t

let () =
  let {cmt_annots; _} = read_cmt Sys.argv.(1) in
  Format.printf "@[<v>digraph {@;";
  (match cmt_annots with
  | Implementation {str_items; _} ->
    str_items |> List.iter (fun {str_desc; _} ->
      match str_desc with
      | Tstr_type (_, ds) ->
        ds |> List.iter (fun {typ_id; typ_loc; typ_kind; typ_manifest; _} ->
          Format.printf "\"%a\" [label=%S];@;"
            Ident.print_with_scope typ_id
            (Ident.name typ_id);
          ignore typ_loc;
          let rec f {ctyp_desc; _} =
            let g = function
              | Ttyp_any -> ()
              | Ttyp_var _ -> ()
              | Ttyp_arrow (_l, a, b) -> f a; f b
              | Ttyp_tuple l -> List.iter f l
              | Ttyp_constr (p, _, args) ->
                Format.printf "\"%a\" -> \"%a\";@;"
                  Ident.print_with_scope typ_id
                  Path.print p;
                (* maybe this wasn't defined before *)
                Format.printf "\"%a\" [label=%S];@;"
                  Path.print p
                  (Path.name p);
                List.iter f args
              | Ttyp_alias (t, _s) -> f t
              | Ttyp_variant (fs, _f, _ls) ->
                fs |> List.iter (fun {rf_desc; _} ->
                  match rf_desc with
                  | Ttag (_n, _b, args) -> List.iter f args
                  | Tinherit t -> f t
                )
              | Ttyp_poly (_vs, t) -> f t
              | Ttyp_package {pack_fields; _} ->
                List.iter (fun (_l, t) -> f t) pack_fields
              (* TODO *)
              | Ttyp_object _ -> ()
              | Ttyp_class _ -> ()
            in
            g ctyp_desc
          in
          match typ_kind with
          | Ttype_variant cs ->
            cs |> List.iter (fun {cd_args; _} ->
              match cd_args with
              | Cstr_tuple ts ->
                List.iter f ts
              | Cstr_record ls ->
                List.map (fun {ld_type; _} -> ld_type) ls |>
                List.iter f
            )
          | Ttype_record ls ->
            List.map (fun {ld_type; _} -> ld_type) ls |>
            List.iter f
          | Ttype_open ->
            ()
          | Ttype_abstract ->
            match typ_manifest with
            | None -> ()
            | Some t -> f t
        )
      (* TODO implement those cases *)
      | Tstr_exception {tyexn_loc; _} -> ignored "exception" tyexn_loc
      | Tstr_typext {tyext_loc; _} -> ignored "typext" tyext_loc
      | Tstr_module {mb_loc; _} -> ignored "module" mb_loc
      | Tstr_recmodule ms ->
        List.iter (fun {mb_loc; _} -> ignored "recmodule" mb_loc) ms
      | Tstr_include {incl_loc; _} -> ignored "include" incl_loc
      | Tstr_class cis ->
        List.iter (fun ({ci_loc; _}, _) -> ignored "class" ci_loc) cis
      | Tstr_class_type cts ->
        List.iter (fun (_, _, {ci_loc; _}) -> ignored "class_type" ci_loc) cts
      (* TODO confirm those can be ignored *)
      | Tstr_eval _ -> ()
      | Tstr_value _ -> ()
      | Tstr_primitive _ -> ()
      | Tstr_modtype _ -> ()
      | Tstr_open _ -> ()
      | Tstr_attribute _ -> ()
    )
  | Interface _ ->
    Format.printf "TODO\n"
  | _ ->
    Format.printf "unsupported format\n");
  Format.printf "}@]@."
