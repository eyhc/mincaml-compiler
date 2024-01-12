(*exemple de code pour utiliser des if imbriqués successifs (comme un for) pour tester l'allocation de resgistres et la génération de l'arm*)
let a = 2 in
let b = 5 in
let res = 0 in
let res =
    if res <= b then
        let res = res + a in
        if res <= b then
            let res = res + a in
            if res <= b then
                let res = res + a in
                if res <= b then
                    if res <= b then
                        res + a
                    else res
                else res
            else res
        else res    
    else res
in print_int res