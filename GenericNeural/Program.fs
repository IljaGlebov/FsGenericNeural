
open System
open GenericNeural

[<EntryPoint>]
let main argv =
    let training = [ [| 0.0; 0.0; 0.0; 0.0 |]
                     
                     [| 1.0; 0.0; 0.0; 0.25 |]
                     [| 0.0; 1.0; 0.0; 0.25 |]
                     [| 0.0; 1.0; 1.0; 0.25 |]
                     
                     [| 1.0; 1.0; 0.0; 0.5 |]
                     [| 1.0; 0.0; 1.0; 0.5 |]
                     [| 0.0; 1.0; 1.0; 0.5 |]
                     
                     [| 1.0; 1.0; 1.0; 1.0 |]
                     ]
    
    let error net =
        training |> List.map (fun set -> let result = Neural.calculate net set
                                         Math.Abs(set.[3] - result.[0]))
                 |> List.map (fun x -> x * x)
                 |> List.sum
                 |> Math.Sqrt
    
    
    let generation0 = Genetic.generatate0 (fun () -> Neural.initSingle 3 3 (fun () -> 1.0)) 1000
    
    let rec calculateNext gen =
        seq {
            yield gen
            yield! Genetic.next gen error |> calculateNext
        }
        
    let (Genetic.Generation (i, gen1000)) = calculateNext generation0 |> Seq.skip 1000 |> Seq.head
    
    let v = gen1000.Head
    
    training |> Seq.iter (fun data -> printfn "Input :%A  Exp : %A : Output : %A "  data.[0..2] data.[3] (Neural.calculate v data) )
  
    0 // return an integer exit code
