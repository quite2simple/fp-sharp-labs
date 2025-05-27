let add (x: float) (y: float) =
    x + y
   
let subtract (x: float) (y: float) =
    x - y

let multiply (x: float) (y: float) =
    x * y

let divide (x: float) (y: float) : float option =
    if y = 0.0 then
        None
    else
        Some (x / y)

let rec factorial (n: int) : int =
    if n < 0 then
        invalidArg "n" "n must be non-negative"
    elif n = 0 then
        1
    else
        n * factorial (n - 1)

let demo () =
    printfn "add: %.2f" (add 1.0 2.0)
    printfn "sub: %.2f" (subtract 1.0 2.0)
    printfn "mult: %.2f" (multiply 1.0 2.0)
    match divide 1.0 2.0 with
    | Some result -> printfn "div: %.2f" result
    | None -> printfn "Cannot divide by zero"
    printfn "fact: %d" (factorial 5)

demo ()