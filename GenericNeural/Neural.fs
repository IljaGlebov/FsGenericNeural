namespace GenericNeural


type Activation = | Sygmoid 
                  | ReLU
                  | Tan
                  | Linear



(* Входные данные коофецетны нейрона от уровня N-1*)
type Synapses = Synapses of double[]


type Neuron = | Input
              | Hidden of Activation * Synapses
              | Output of Activation * Synapses
              with static member replaceSyspses n s = match n with | Input -> Input
                                                                   | Hidden (a, _) -> Hidden(a, Synapses s)
                                                                   | Output (a, _) -> Output(a, Synapses s)
                                                                   
                   static member trySynapsis n = match n with | Input -> None
                                                              | Hidden (_, Synapses a) 
                                                              | Output (_, Synapses a) -> Some a
type Layer = Layer of Neuron[]

type Net = Net of Layer[]

module Neural =
    
    open System
    
    let private multiply (v1, v2) = v1 * v2
    
    
    
    let private activate activation v =
        match activation with
        | Sygmoid -> 1.0 / (1.0 + Math.Exp(-v))
        | ReLU -> Math.Max(double 0, v)
        | Tan -> (Math.Exp(v) - Math.Exp(-v)) / (Math.Exp(v) + Math.Exp(-v))
        | Linear -> v
        
    let private calculateLayer (Layer neurons) (input : double[]) =
        neurons |> Array.mapi (fun i n -> match n with | Input -> input.[i]
                                                       | Hidden (activation, Synapses lst) 
                                                       | Output (activation, Synapses lst) -> Array.zip lst input |> Array.map multiply |> Array.sum |> activate activation )
                
    let calculate (Net net) (input : double[]) =
        net |> Array.fold (fun acc layer -> calculateLayer layer acc) input
        
    
    
    let initSingle inputs hiddenCount (generator : unit -> double) =

        let createHidden() =
            Array.init inputs (fun _ -> Hidden(Sygmoid, Synapses (Array.init inputs (fun _ -> generator())))) |> Layer

        let hidden = Array.init hiddenCount (fun _ -> createHidden())

        seq {
            yield Array.init inputs (fun _ -> Input) |>  Layer
            
            yield! hidden
            
            yield  Layer [|Output(Sygmoid, Synapses [|generator(); generator(); generator()|]) |] 
        } |> Seq.toArray |> Net
