(* main module *)

(* exceptions *)
exception Unknown_switch of string
exception Print of string
exception Print_help
exception Print_usage
exception Not_enough_arguments
exception Too_many_arguments

(* type submodules *)
module Store = struct
    type t =
        One of string ref
        | One_opt of string option ref
        | Maybe of string ref
        | Maybe_opt of string option ref
        | Some of string list ref
        | Any of string list ref
    let apply_single_store store arg =
        match store with
            One r
            | Maybe r -> r := arg;  true
            | One_opt r
            | Maybe_opt r -> r := Some arg; true
            | Some r
            | Any r -> r := List.append !r [arg]; false
end
module Effect = struct
    type t =
        Rest
        | Print_help
        | Print_usage
        | Print_string of string
        | Set_bool of bool ref
        | Inc_int of int ref
end

(* command description *)
type cmdopt = {
    effect: Effect.t;
    switches: string list;
    help: string option
}
type cmdarg = {
    metavar: string;
    store: Store.t;
    help: string option
}
type cmd = {
    name: string;
    opts: cmdopt list;
    args: cmdarg list;
    synopsis: string option;
    epilogue: string option
}

(* internal functions *)
let first_chars str len =
    String.sub str 0 len
let string_after str pos =
    String.sub str pos @@ (String.length str) - pos
let switch_prefix = "-"
let is_switch arg =
    first_chars arg (String.length switch_prefix) = switch_prefix
let strip_switch sw =
    if is_switch sw then
        let sw = string_after sw 1 in
        if is_switch sw then
            string_after sw 1
        else
            sw
    else
        raise @@ Invalid_argument ""
let prefix_switch sw =
    String.concat "" [switch_prefix; sw]
let bracketize s =
    String.concat "" ["["; s; "]"]
let lookup_cmdopt_opt sw opts =
    List.find_opt
        (fun opt ->
            List.exists
                (fun switch ->
                    switch = strip_switch sw) opt.switches) opts
let lookup_cmdopt sw opts =
    match lookup_cmdopt_opt sw opts with
        Some opt ->
            opt
        | None ->
            raise @@ Unknown_switch sw
let known_switch sw opts =
    match lookup_cmdopt_opt sw opts with
        None -> false
        | _ -> true
(* public functions *)
let usage cmd =
    Printf.sprintf
        "usage: %s [OPTION ...] [ARG ...]"
        cmd.name
let help cmd =
    let switches_width =
        List.fold_left
            (fun w opt ->
                let s =
                    String.concat ", "
                    @@ List.map prefix_switch
                        opt.switches in
                max (String.length s) w)
            0
            cmd.opts in
    let arguments_width =
        List.fold_left
            (fun w arg ->
                max (String.length arg.metavar) w)
            0
            cmd.args in
    String.concat "\n\n"
        @@ List.filter
            (fun s -> String.trim s != "")
            [
                usage cmd;
                Option.fold ~none:"" ~some:String.trim cmd.synopsis;
                String.concat "\n"
                    [
                        "arguments:";
                        String.concat "\n"
                        @@ List.map
                            (fun arg ->
                                String.concat ""
                                    [
                                        "  ";
                                        Printf.sprintf "%-*s"
                                            arguments_width
                                            arg.metavar;
                                        "  ";
                                        Option.fold
                                            ~none:""
                                            ~some:String.trim
                                            arg.help
                                    ])
                            cmd.args
                    ];
                String.concat "\n"
                    [
                        "options:";
                        String.concat "\n"
                        @@ List.map
                            (fun opt ->
                                String.concat ""
                                    [
                                        "  ";
                                        Printf.sprintf "%-*s"
                                            switches_width
                                        @@ String.concat ", "
                                        @@ List.map prefix_switch
                                        opt.switches;
                                        "  ";
                                        Option.fold
                                            ~none:""
                                            ~some:String.trim
                                            opt.help
                                    ])
                            cmd.opts
                    ];
                Option.fold ~none:"" ~some:String.trim cmd.epilogue
            ]
let rec apply_cmd argv ?(args=[]) cmd =
    match argv with
        [] ->
            let rec apply_args args cmdargs =
                let n_args = List.length args in
                let min_n_args =
                    List.fold_left
                        (fun n arg ->
                            max n @@
                            match arg.store with
                                One _ | One_opt _ | Some _ -> 1
                                | Maybe _ | Maybe_opt _ | Any _ -> 0)
                        0
                        cmdargs in
                match cmdargs, args with
                    [], [] -> ()
                    | [], _ :: _ -> raise Too_many_arguments
                    | _ :: _, [] -> raise Not_enough_arguments
                    | cmdarg :: cmdargs_tl, arg :: args ->
                        if Store.apply_single_store cmdarg.store arg
                            || n_args <= min_n_args then
                                apply_args args cmdargs_tl
                        else
                            apply_args args cmdargs
                    in
            apply_args args cmd.args
        | arg :: argv ->
            match arg with
                | _ when is_switch arg && known_switch arg cmd.opts ->
                    let opt = lookup_cmdopt arg cmd.opts in
                    begin match opt.effect with
                        Rest ->
                            apply_cmd [] cmd ~args:(args @ argv)
                        | Print_help ->
                            raise @@ Print_help
                        | Print_usage ->
                            raise @@ Print_usage
                       | Print_string s ->
                            raise @@ Print s
                        | Set_bool b ->
                            b := true;
                            apply_cmd argv cmd ~args:args
                        | Inc_int i ->
                            i := !i + 1;
                            apply_cmd argv cmd ~args:args
                    end
                | _ when is_switch arg ->
                    raise @@ Unknown_switch arg
                | _ ->
                    apply_cmd argv cmd ~args:(args @ [arg])