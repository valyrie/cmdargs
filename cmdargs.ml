(* main module *)

exception Unknown_switch of string
exception Print of string
exception Print_help
exception Print_usage
exception Not_enough_arguments
exception Too_many_arguments
exception Argument_looks_like_a_switch
module Switch = struct
    let first_chars str len =
        String.sub str 0 len
    let string_after str pos =
        String.sub str pos @@ (String.length str) - pos
    let rec is_switch sw prefixes =
        match prefixes with
            [] -> false
            | prefix :: prefixes ->
                if first_chars sw @@ String.length prefix = prefix then
                    true
                else
                    is_switch sw prefixes
    let rec strip_prefix sw prefixes =
        match prefixes with
            [] -> raise @@ Invalid_argument ""
            | prefix :: prefixes ->
                if first_chars sw @@ String.length prefix = prefix then
                    string_after sw @@ String.length prefix
                else
                    strip_prefix sw prefixes
    let prefix prefix sw =
        String.concat "" [prefix; sw]
end
module Opt = struct
    module Effect = struct
        module Argument = struct
            module Store = struct
                type t =
                    Set_string of string ref
                    | Set_string_opt of string option ref
                    | Append_string of string list ref
            end
            type t = {
                name: string;
                store: Store.t
            }
        end
        type t =
            Rest
            | Print_help
            | Print_usage
            | Print_string of string
            | Set_bool of bool ref
            | Inc_int of int ref
            | Store of Argument.t list
    end
    type t = {
        effect: Effect.t;
        switches: string list;
        in_usage: bool;
        help: string option
    }
    let lookup_sw_opt sw prefixes opts =
        let sw = Switch.strip_prefix sw prefixes in
        List.find_opt
            (fun opt ->
                List.exists
                    (fun s ->
                        s = sw)
                    opt.switches)
            opts
    let lookup_sw sw prefixes opts =
        match lookup_sw_opt sw prefixes opts with
            None -> raise @@ Unknown_switch sw
            | Some opt -> opt
    let known sw prefixes opts =
        match lookup_sw_opt sw prefixes opts with
            None -> false
            | _ -> true
    let display prefix opt =
        match opt.effect with
            Rest
            | Print_help | Print_usage | Print_string _
            | Set_bool _ | Inc_int _ ->
                String.concat ""
                    ["["; prefix; List.hd opt.switches; "]"]
            | Store ls ->
                String.concat ""
                    ["[";
                        String.concat " "
                        @@ List.cons
                            (Switch.prefix prefix @@ List.hd opt.switches)
                        @@ List.map
                            (fun (arg: Effect.Argument.t) -> arg.name)
                            ls; "]"]
    let display_list prefix opts =
        String.concat " "
        @@ List.flatten
            [
                List.map (display prefix)
                @@ List.filter
                    (fun opt ->
                        match opt.effect with
                            Rest -> false
                            | _ -> opt.in_usage)
                    opts;
                if List.exists
                    (fun opt -> not opt.in_usage)
                    opts then
                        ["[OPTION ...]"]
                else
                    [];
                List.map (display prefix)
                @@ Option.fold
                    ~none:[]
                    ~some:(fun x -> [x])
                @@ List.find_opt
                    (fun opt ->
                        match opt.effect with
                            Rest -> opt.in_usage
                            | _ -> false)
                    opts
            ]
    let help prefix opts =
        let switches_width =
            List.fold_left
                (fun w opt ->
                    let s =
                        String.concat ", "
                        @@ List.map (Switch.prefix prefix)
                            opt.switches in
                    max (String.length s) w)
                0
                opts in
        if List.exists
            (fun opt ->
                Option.is_some opt.help || not opt.in_usage)
            opts then
                String.concat "\n"
                @@ List.flatten
                    [
                        ["options:"];
                        List.map
                            (fun opt ->
                                String.concat ""
                                    [
                                        "  ";
                                        Printf.sprintf "%-*s"
                                            switches_width
                                        @@ String.concat ", "
                                        @@ List.map (Switch.prefix prefix)
                                        opt.switches;
                                        "  ";
                                        Option.fold
                                           ~none:"(undocumented)"
                                            ~some:String.trim
                                            opt.help
                                    ])
                            opts
                    ]
        else
            ""
end
module Arg = struct
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
    type t = {
        name: string;
        store: Store.t;
        help: string option
    }
    let display arg =
        let name = arg.name in
        match arg.store with
            One _ | One_opt _ -> name
            | Maybe _ | Maybe_opt _ ->
                String.concat "" ["["; name; "]"]
            | Some _ ->
                String.concat "" [name; " ["; name; " ...]"]
            | Any _ ->
                String.concat "" ["["; name; " ...]"]
    let display_list args =
        String.concat " "
        @@ List.map display
            args
    let help args =
        let arguments_width =
            List.fold_left
                (fun w arg ->
                    max (String.length arg.name) w)
                0
                args in
        if List.exists
            (fun arg ->
                Option.is_some arg.help)
            args then
                String.concat "\n"
                @@ List.flatten
                    [
                        ["arguments:"];
                        List.map
                            (fun arg ->
                                String.concat ""
                                    [
                                        "  ";
                                        Printf.sprintf "%-*s"
                                            arguments_width
                                            arg.name;
                                        "  ";
                                        String.trim
                                        @@ Option.get arg.help
                                    ])
                        @@ List.filter
                            (fun arg ->
                                Option.is_some arg.help)
                            args
                    ]
        else
            ""
end
type cmd = {
    name: string;
    switch_prefixes: string list;
    opts: Opt.t list;
    args: Arg.t list;
    synopsis: string option;
    help: string option
}
let usage cmd =
    Printf.sprintf
        "usage: %s"
        @@ String.concat " "
        @@ List.cons cmd.name
        @@ List.filter
            (fun s -> String.trim s != "")
            [
                Opt.display_list (List.hd cmd.switch_prefixes) cmd.opts;
                Arg.display_list cmd.args
            ]
let help cmd =
    String.concat "\n\n"
        @@ List.filter
            (fun s -> String.trim s != "")
            [
                usage cmd;
                Option.fold ~none:"" ~some:String.trim cmd.synopsis;
                Arg.help cmd.args;
                Opt.help (List.hd cmd.switch_prefixes) cmd.opts;
                Option.fold ~none:"" ~some:String.trim cmd.help
            ]
let rec apply_cmd argv ?(args=[]) cmd =
    match argv with
        [] ->
            let rec apply_args args cmdargs =
                let n_args = List.length args in
                let min_n_args =
                    List.fold_left
                        (fun n (arg: Arg.t) ->
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
                        if Arg.Store.apply_single_store cmdarg.store arg
                            || n_args <= min_n_args then
                                apply_args args cmdargs_tl
                        else
                            apply_args args cmdargs
                    in
            apply_args args cmd.args
        | arg :: argv ->
            match arg with
                | _ when
                    Switch.is_switch arg cmd.switch_prefixes
                    && Opt.known arg cmd.switch_prefixes cmd.opts ->
                        let opt =
                            Opt.lookup_sw
                                arg cmd.switch_prefixes cmd.opts in
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
                            | Store ls ->
                                if List.length argv < List.length ls then
                                    raise @@ Not_enough_arguments
                                else
                                    List.iter
                                        (fun ((s: Opt.Effect.Argument.t), arg) ->
                                            match s.store with
                                                Set_string r -> r := arg
                                                | Set_string_opt r -> r := Some arg
                                                | Append_string r -> r := List.append !r [arg])
                                    @@ List.combine
                                        ls
                                    @@ List.filteri
                                        (fun i _ ->
                                            i < List.length ls)
                                        argv;
                                    let argv = 
                                        List.filteri
                                            (fun i _ ->
                                                i >= List.length ls)
                                            argv in
                                    apply_cmd argv cmd ~args:args
                        end
                | _ when Switch.is_switch arg cmd.switch_prefixes ->
                    raise @@ Unknown_switch arg
                | _ ->
                    apply_cmd argv cmd ~args:(args @ [arg])