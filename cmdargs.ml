(* main module *)

exception Unknown_switch of string
module Effect = struct
    type t =
        Rest
        | Set_bool of bool ref
        | Inc_int of int ref
end
type cmdopt = {
    effect: Effect.t;
    switches: string list;
    help: string option
}
type cmd = {
    name: string;
    opts: cmdopt list;
    synopsis: string option;
    help: string option
}
let switch_prefix = "-"
let is_switch arg =
    Str.first_chars arg (String.length switch_prefix) = switch_prefix
let strip_switch sw =
    if is_switch sw then
        let sw = Str.string_after sw 1 in
        if is_switch sw then
            Str.string_after sw 1
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
let usage cmd =
    Printf.sprintf
        "usage: %s [options] [arg ...]"
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
            cmd.opts
            in
    String.concat "\n\n"
        @@ List.filter
            (fun s -> String.trim s != "")
            [
                usage cmd;
                Option.fold ~none:"" ~some:String.trim cmd.synopsis;
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
                Option.fold ~none:"" ~some:String.trim cmd.help
            ]
let rec apply_cmd argv ?(args=[]) cmd =
    match argv with
        [] ->
            args
        | arg :: argv ->
            match arg with
                | _ when is_switch arg && known_switch arg cmd.opts ->
                    let opt = lookup_cmdopt arg cmd.opts in
                    begin match opt.effect with
                        Rest ->
                            apply_cmd [] cmd ~args:(args @ argv)
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