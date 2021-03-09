namespace GenericNeural


(* Мутации
   Кросинг
   Сознание нового поколения  *)
module Genetic =
    open System
    open GenericNeural.Neural
    open GenericNeural.NeuralTransformation
    
    (* Поколение *)
    type Generation<'T> = Generation of int * 'T list 
    
    let private random = Random()
    let private min (a : int) (b : int) = Math.Min(a, b)

    let private rnd min max = random.Next(min, max)
    let private rndDouble min max = min + random.NextDouble() * (max - min)
    
    type CrossingPoint = CrossingPoint of layer : int * neuron : int * synapses : int 
    
    (* Два массива и точка кроссовера + функция замены элемента в точке кроссовера *)
    let private crossArray (s1 : 'a[]) (s2 : 'a[]) p (replacePoint : 'a -> 'a -> 'a * 'a)  =
        let child1, child2 = Array.copy s1, Array.copy s2
            
        let maxLenght = Math.Max(s1.Length, s2.Length)
        
        for i = p to maxLenght do
            if i < s1.Length && i < child2.Length then
                child2.[i] <- s1.[i]
            if i < s2.Length && i < child1.Length then
                child1.[i] <- s2.[i]
        let (rep1, rep2) = replacePoint s1.[p] s2.[p]
        
        child1, child2
   
    let private choosePoint (Net net) =
        let layerPoint = rnd 1 net.Length 
        let (Layer layer) = net.[layerPoint] 
        let neuronPoint = rnd 0 layer.Length 
        let neuron = layer.[neuronPoint]
        let synapsisPoint = neuron |> Neuron.trySynapsis |> Option.get |> Array.length |> (fun l -> rnd 0 l) 
        CrossingPoint (layerPoint, neuronPoint, synapsisPoint)
        
    (* Обновить вес случайно выбранной связи *)
    let private updateNeuronSynapsisWeight point v net =
        
        let (CrossingPoint(layer, neuron, synapsis)) = point
        
        let m = UpdateLayer(layer, UpdateNeuron(neuron, SetSynapsis(synapsis, v)))
        
        net |> NeuralTransformation.update m
        
    (* Обновить функцию активации выбранной связи *)
    let private updateNeuronActivation point activation net =
        
        let (CrossingPoint(layer, neuron, synapsis)) = point
        
        let m = UpdateLayer(layer, UpdateNeuron(neuron, ChangeActivation activation))
        
        net |> NeuralTransformation.update m
        
        
    (* Скрещивание двух сетей *)
    let private crossover (Net net1) (Net net2) =
        
        let (|HasSynapsis|_|) = function | Hidden (_, s) | Output (_, s) -> Some s
                                          | _ -> None
        
        let layerPoint =
            let size = min net1.Length net2.Length
            (* Входной слой не меняется *)
            rnd 1 size
            
        let layerAt = Array.item layerPoint >> (function Layer ns -> ns)
            
        let neuronPoint =
            let lenghtOfLayer = layerAt >> Array.length
            let size = min (lenghtOfLayer net1) (lenghtOfLayer net2)
            rnd 0 size
            
        let neuronOf = layerAt >> Array.item neuronPoint
            
        let synapsisPoint =
            let synapsisOf = neuronOf >> (function | Hidden (_, Synapses arr) | Output (_, Synapses arr) -> arr | _ -> failwith "")
            let lengthOfSynapsis = synapsisOf >> Array.length
            
            let size = min (lengthOfSynapsis net1) (lengthOfSynapsis net2)
            rnd 1 size 
          
        let crossSynapsis p (s1 : double[]) (s2 : double[])  =
            let child1, child2 = Array.copy s1, Array.copy s2
            
            let maxLenght = Math.Max(s1.Length, s2.Length)
            
            for i = p to maxLenght do
                if i < s1.Length && i < child2.Length then
                    child2.[i] <- s1.[i]
                if i < s2.Length && i < child1.Length then
                    child1.[i] <- s2.[i]
            
            child1, child2
            
        let crossNerons (Layer neurons1) (Layer neurons2) p (crossSynapsis  : double[] -> double[] -> (double[] * double[])) =
            let replace a b =
                match a, b with
                    | HasSynapsis (Synapses sy1), HasSynapsis(Synapses sy2) ->
                        (* Дошли то точки кроссинга*)
                        let neuronCross1, neuronCross2 = crossSynapsis sy1 sy2
                        
                        Neuron.replaceSyspses b neuronCross2, Neuron.replaceSyspses a neuronCross1
                        
                    | _ -> a, b
            
            crossArray neurons1 neurons2 p replace |> (function (a, b) -> (Layer a, Layer b))
            
                      
        crossArray net1 net2 layerPoint (fun a b -> crossNerons a b neuronPoint (crossSynapsis synapsisPoint))
    
    
    type private Mutation = | ChangeWeight // Сменить вес
                            | ChangeActivation
    
    let private applyMutation mutation net =
        match mutation with
        | ChangeWeight ->
            let point = choosePoint net
            let v = rndDouble -5.0 5.0
            net |> updateNeuronSynapsisWeight point v
        | ChangeActivation ->
            let point = choosePoint net
            
            let activation =
                match rnd 0 4 with
                | 0 -> Activation.Linear
                | 1 -> Activation.Sygmoid
                | 2 -> Activation.Tan
                | 3 -> Activation.ReLU
                | _ -> failwith ""
                
            net |> updateNeuronActivation point activation
            
    (* Создать нулевое поколение *)            
    let generatate0 (generator : unit -> 't) count =
        let data = List.init count (fun _ -> generator())
        Generation(0, data)
        
    let private randomPairs list =
        let arr = list |> List.toArray
        
        Seq.initInfinite (fun _ -> rnd 0 arr.Length, rnd 0 arr.Length)
            |> Seq.filter (fun (i1, i2) -> i1 <> i2)
            |> Seq.map (fun (i1, i2) -> arr.[i1], arr.[i2])
        
    let private randomItems list =
        let arr = list |> List.toArray
        
        Seq.initInfinite (fun _ -> rnd 0 arr.Length)
            |> Seq.map (fun i -> arr.[i])
       
    let next generation fintes =
        let (Generation (i, data)) = generation
        
        let size = float data.Length * 0.1 |> int
        
        seq {
                            
            yield! data
            
            (* кроссинг *)
            yield! data |> randomPairs
                        |> Seq.take size
                        |> Seq.map(fun (v1, v2) ->  crossover v1 v2)
                        |> Seq.collect (fun (n1,n2) -> [Net n1; Net n2])

            (* мутации *)
            
            yield! data |> randomItems
                        |> Seq.take size
                        |> Seq.map (applyMutation ChangeWeight)
                        
            yield! data |> randomItems
                        |> Seq.take size
                        |> Seq.map (applyMutation ChangeActivation)
                        
                        
        }   |> Seq.sortBy fintes
            |> Seq.take data.Length
            |> Seq.toList
            |> (fun lst -> Generation (i + 1, lst))