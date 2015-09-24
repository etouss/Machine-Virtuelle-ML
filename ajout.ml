let rec ajout_sort n liste nl = match liste with
    | [] -> List.rev (n::nl)
    | a::b -> if a > n then ajout_sort n b (a::nl)
              else List.rev(n::nl) @ liste
;;
