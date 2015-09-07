type OpCode =
  | Halt = 0x01
  | IntPrint = 0x02
  | IntPush = 0x03
  | Add = 0x04

type MachineState =
 {
   IP : int
   Stack : byte list
 }

[<EntryPoint>]
let main argv =

    let rec cpu (instructions:int[]) (state:MachineState) =
      match state.IP with
      | ip when ip < instructions.Length ->
        let op = instructions.[ip]
        match (op |> enum) with
        | OpCode.Halt -> printfn "HALTED"
        | OpCode.IntPrint ->
          match state.Stack with
          | a::b::c::d::xs ->
            printfn "%d" (System.BitConverter.ToInt32([|a;b;c;d|], 0))
            cpu instructions {state with IP = state.IP + 1; Stack = xs}
          | _ -> printfn "No int on stack. Aborting."
        | OpCode.IntPush ->
          let value = instructions.[ip+1]
          let bytes = System.BitConverter.GetBytes(value) |> Array.rev
          cpu instructions {state with IP = state.IP + 2; Stack = bytes.[3]::bytes.[2]::bytes.[1]::bytes.[0]::state.Stack}
        | OpCode.Add ->
          match state.Stack with
          | a::b::c::d::e::f::g::h::xs ->
          let value1 = System.BitConverter.ToInt32([|a;b;c;d|],0)
          let value2 = System.BitConverter.ToInt32([|e;f;g;h|],0)
          let sum = value1 + value2
          let bytes = System.BitConverter.GetBytes(sum) |> Array.rev
          cpu instructions {state with IP = state.IP + 1; Stack = bytes.[3]::bytes.[2]::bytes.[1]::bytes.[0]::xs}
          | _ -> printfn "Two ints not found on stack. Aborting."
        | _ -> invalidOp "Invalid opcode"
      | _ -> printf "DONE"

    let program =
      [|
        int OpCode.IntPush
        10
        int OpCode.IntPush
        90
        int OpCode.IntPush
        23
        int OpCode.Add
        int OpCode.Add
        int OpCode.IntPrint
      |]

    printfn "Running..."

    cpu program {IP = 0; Stack = []}

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
