type OpCode =
  | Halt = 0x01
  | IntPrint = 0x02
  | IntPush = 0x03
  | Add = 0x04

type MachineState =
 {
   IP : int
   Stack : System.Collections.Generic.Stack<byte>
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
          let value = [|state.Stack.Pop(); state.Stack.Pop(); state.Stack.Pop(); state.Stack.Pop()|]          
          printfn "%d" (System.BitConverter.ToInt32(value, 0))
          cpu instructions {state with IP = state.IP + 1}
        | OpCode.IntPush ->
          let value = instructions.[ip+1]
          let bytes = System.BitConverter.GetBytes(value) |> Array.rev
          bytes |> Seq.iter state.Stack.Push
          cpu instructions {state with IP = state.IP + 2}
        | OpCode.Add ->
          let value1 = System.BitConverter.ToInt32([|state.Stack.Pop(); state.Stack.Pop(); state.Stack.Pop(); state.Stack.Pop()|],0)
          let value2 = System.BitConverter.ToInt32([|state.Stack.Pop(); state.Stack.Pop(); state.Stack.Pop(); state.Stack.Pop()|],0)
          let sum = value1 + value2
          let bytes = System.BitConverter.GetBytes(sum) |> Array.rev
          bytes |> Seq.iter state.Stack.Push
          cpu instructions {state with IP = state.IP + 1}
        | _ -> invalidOp "Invalid opcode"
      | _ -> printf "DONE"

    let program =
      [|
        int OpCode.IntPush
        5
        int OpCode.IntPush
        7
        int OpCode.Add
        int OpCode.IntPrint
      |]

    printfn "Running..."

    cpu program {IP = 0; Stack = System.Collections.Generic.Stack<byte>()}

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
