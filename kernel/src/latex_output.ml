
open! Core
open! Import

let xcolor_of : Format.Color.t -> string = function
  | Black -> "black"
  | Red -> "red"
  | Green -> "green"
  | Yellow -> "yellow"
  | Blue -> "blue"
  | Magenta -> "magenta"
  | Cyan -> "cyan"
  | White -> "white"
  | Default -> "black"
  | Gray | Bright_black -> "gray"
  | Bright_red -> "red"
  | Bright_green -> "green"
  | Bright_yellow -> "yellow"
  | Bright_blue -> "blue"
  | Bright_magenta -> "magenta"
  | Bright_cyan -> "cyan"
  | Bright_white -> "white"
  | RGB6 _ -> "red"
  | Gray24 _ -> "red"
;;

let codes_of_style : Format.Style.t -> string = function
  | Reset -> ""
  | Bold -> "\\bfseries"
  | Dim -> "\\color{gray}"
  | Underline  -> "\\itshape"
  | Emph -> "\\itshape"
  | Blink -> "\\itshape"
  | Inverse -> "\\itshape"
  | Hide -> "\\itshape"
  | Fg c | Foreground c -> Printf.sprintf "\\color{%s}" (xcolor_of c)
  | Bg c | Background c -> Printf.sprintf "\\colorbox{%s}"  (xcolor_of c)
;;

let apply_styles ?(drop_leading_resets = false) (styles : Format.Style.t list) str =
  let styles =
    if drop_leading_resets
    then
      List.drop_while styles ~f:(function
        | Reset -> true
        | _ -> false)
    else (
      match styles with
      | [] -> []
      | Reset :: _ -> styles
      | _ :: _ -> Reset :: styles)
  in
  let escape c str = String.substr_replace_all str ~pattern:c ~with_:(Printf.sprintf "\\%s" c) in 
  let replace c r str = String.substr_replace_all str ~pattern:c ~with_:r in 
  match styles with
  | [] -> str
  | _ ->
    sprintf
      "|%s{\strut{}%s}|"
      (List.map styles ~f:codes_of_style |> String.concat)
      (str |> replace "~" "\\textasciitilde" |> replace "^" "\\textasciicircum" |> replace "\\" "\\textasciibackslash" |> escape "_" |> escape "@" |> escape "{" |> escape "}" |> escape "#" |> escape "$" |> escape "%" |> escape "%"
          )
;;

module Rule = struct
  let apply text ~(rule : Format.Rule.t) ~refined =
    let only_whitespace =
      (not (String.is_empty text)) && String.for_all text ~f:Char.is_whitespace
    in
    let text_style : Format.Style.t list =
      if List.is_empty rule.styles
      then []
      else (
        match refined, only_whitespace with
        | true, _ -> [ Reset ]
        | false, true -> Inverse :: rule.styles
        | false, false -> rule.styles)
    in
    sprintf
      "%s%s%s"
      (apply_styles rule.pre.styles rule.pre.text)
      (apply_styles text_style text)
      (apply_styles rule.suf.styles rule.suf.text)
  ;;
end

let print_header ~(rules : Format.Rules.t) ~file_names:(prev_file, next_file) ~print =
  let print_line (file : File_name.t) rule =
    print (Rule.apply (File_name.display_name file) ~rule ~refined:false)
  in
  print "\\begin{minted}[escapeinside=||]{text}\n" ;
  print_line prev_file rules.header_prev;
  print_line next_file rules.header_next;
;;

let print
      ~print_global_header
      ~file_names:((prev_file, _) as file_names)
      ~(rules : Format.Rules.t)
      ~print
      ~location_style
      hunks
  =
  let f_hunk_break hunk =
    Format.Location_style.sprint
      location_style
      hunk
      ~prev_filename:(File_name.display_name prev_file)
      ~rule:(Rule.apply ~rule:rules.hunk ~refined:false)
    |> print
  in
  if print_global_header then print_header ~rules ~file_names ~print;
  Hunks.iter' ~f_hunk_break ~f_line:print hunks;
  print "\\end{minted}\n" ;
;;
