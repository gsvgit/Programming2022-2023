module Inline

let inline f x y = x + y

let _do () =
    let r2 = f 1 2
    let r1 = f 1.3 1.4
    printfn $"%A{r1}; %A{r2}"
