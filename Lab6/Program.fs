// Calculator - A functional calculator implementation in F#
// Supports basic arithmetic, exponentiation, square roots, and trigonometric functions

open System

// Types
type Operation =
    | Add
    | Subtract
    | Multiply
    | Divide
    | Power
    | Sqrt
    | Sin
    | Cos
    | Tan
    | History
    | Exit
    | Cancel

let operationToString = function
    | Add -> "+"
    | Subtract -> "-"
    | Multiply -> "*"
    | Divide -> "/"
    | Power -> "^"
    | Sqrt -> "sqrt"
    | Sin -> "sin"
    | Cos -> "cos"
    | Tan -> "tan"
    | History -> "history"
    | Exit -> "exit"
    | Cancel -> "c"

// Domain
type CalculationResult = Result<float, string>
type Calculation = { Operation: Operation; Operands: float list; Result: CalculationResult }

type CalculatorState = {
    History: Calculation list
    CurrentOperation: Operation option
    CurrentOperands: float list
}

// Pure functions for calculations
let add a b = a + b
let subtract a b = a - b
let multiply a b = a * b
let divide a b = if b <> 0.0 then Ok(a / b) else Error "Division by zero"
let power a b = Math.Pow(a, b)
let sqrt x = if x >= 0.0 then Ok(Math.Sqrt(x)) else Error "Cannot calculate square root of a negative number"

let performOperation op operands =
    match op, operands with
    | Add, [a; b] -> Ok(add a b)
    | Subtract, [a; b] -> Ok(subtract a b)
    | Multiply, [a; b] -> Ok(multiply a b)
    | Divide, [a; b] -> divide a b
    | Power, [a; b] -> Ok(power a b)
    | Sqrt, [x] -> sqrt x
    | Sin, [x] -> Ok(Math.Sin(x * Math.PI / 180.0)) // Convert degrees to radians
    | Cos, [x] -> Ok(Math.Cos(x * Math.PI / 180.0))
    | Tan, [x] -> Ok(Math.Tan(x * Math.PI / 180.0))
    | _ -> Error "Invalid operation or number of operands"

// State management
let initialState = { History = []; CurrentOperation = None; CurrentOperands = [] }

let updateState state operation operands result =
    let calculation = { Operation = operation; Operands = operands; Result = result }
    { state with 
        History = calculation :: state.History
        CurrentOperation = None
        CurrentOperands = []
    }

// I/O functions
let displayWelcome () =
    printfn "Welcome to the F# Calculator!"
    printfn "Available operations:"
    printfn "  + : Addition (a + b)"
    printfn "  - : Subtraction (a - b)"
    printfn "  * : Multiplication (a * b)"
    printfn "  / : Division (a / b)"
    printfn "  ^ : Exponentiation (a ^ b)"
    printfn "  sqrt : Square root (√x)"
    printfn "  sin : Sine of angle in degrees"
    printfn "  cos : Cosine of angle in degrees"
    printfn "  tan : Tangent of angle in degrees"
    printfn "  history : Show calculation history"
    printfn "  exit : Exit the calculator"
    printfn "  c : Cancel current operation"
    printfn ""

let displayPrompt () =
    printf "> "
    Console.ReadLine().Trim().ToLower()

let displayResult result =
    match result with
    | Ok value -> printfn "Result: %f" value
    | Error msg -> printfn "Error: %s" msg

let displayHistory history =
    printfn "\nCalculation History:"
    printfn "-----------------"
    history
    |> List.rev
    |> List.iteri (fun i calc ->
        let operands = calc.Operands |> List.map string |> String.concat ", "
        let resultStr = match calc.Result with | Ok v -> string v | Error e -> $"Error: {e}"
        printfn "%d. %s %s = %s" (i + 1) (operationToString calc.Operation) operands resultStr)
    printfn ""

// Main loop
let rec processInput state =
    match state.CurrentOperation with
    | None ->
        // Main command input
        printfn "\nEnter operation or command (+, -, *, /, ^, sqrt, sin, cos, tan, history, exit, c):"
        let input = displayPrompt()
        
        let operation = 
            match input with
            | "+" -> Some Add
            | "-" -> Some Subtract
            | "*" -> Some Multiply
            | "/" -> Some Divide
            | "^" -> Some Power
            | "sqrt" -> Some Sqrt
            | "sin" -> Some Sin
            | "cos" -> Some Cos
            | "tan" -> Some Tan
            | "history" -> 
                displayHistory state.History
                None
            | "exit" -> Some Exit
            | "c" -> 
                printfn "No operation to cancel."
                None
            | _ -> 
                printfn "Invalid operation. Please try again."
                None
        
        match operation with
        | Some Exit -> 
            printfn "Goodbye!"
            () // Exit the program
        | Some op -> 
            processInput { state with CurrentOperation = Some op }
        | None -> processInput state
            
    | Some op ->
        // Get operands based on operation
        let operandCount = 
            match op with
            | Add | Subtract | Multiply | Divide | Power -> 2
            | Sqrt | Sin | Cos | Tan -> 1
            | _ -> 0 // Shouldn't happen as we control the operations
        
        let rec getOperands operandsNeeded currentOperands =
            if operandsNeeded = 0 then
                List.rev currentOperands
            else
                let prompt = $"Enter operand {List.length currentOperands + 1} (or 'c' to cancel): "
                printf "%s" prompt
                let input = Console.ReadLine().Trim().ToLower()
                
                match input with
                | "c" -> 
                    printfn "Operation cancelled."
                    []
                | _ ->
                    match Double.TryParse(input) with
                    | true, num -> 
                        getOperands (operandsNeeded - 1) (num :: currentOperands)
                    | _ ->
                        printfn "Invalid number. Please try again."
                        getOperands operandsNeeded currentOperands
        
        let operands = getOperands operandCount []
        
        if operands.Length = operandCount then
            // Perform the calculation
            let result = performOperation op operands
            displayResult result
            
            // Update state with the new calculation
            let newState = updateState state op operands result
            processInput newState
        else
            // Operation was cancelled
            processInput { state with CurrentOperation = None }

[<EntryPoint>]
let main _ =
    displayWelcome ()
    processInput initialState
    0
