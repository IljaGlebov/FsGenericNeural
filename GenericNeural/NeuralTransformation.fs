namespace GenericNeural


module NeuralTransformation =
    open GenericNeural.Neural
        
    type Transformation =     | ChangeActivation of Activation
                              | SetSynapsis of int * double
                              
    type LayerTransformation = | UpdateNeuron of int * Transformation
        
    type NetTransformation = | UpdateLayer of int * LayerTransformation
    
    let private (|Copy|) arr = Array.copy arr
    
    let private updateNeuron neuron manipulation =
        match manipulation with
        | ChangeActivation f ->
            match neuron with
            | Input -> Input
            | Hidden (_, s) -> Hidden(f, s)
            | Output (_, s) -> Output(f, s)
        | SetSynapsis (index, v) ->
            match neuron with
            | Input -> Input
            | Hidden (f, Synapses(Copy s)) -> s.[index] <- v
                                              Hidden(f, Synapses s)
            | Output (f, Synapses(Copy s)) -> s.[index] <- v
                                              Output(f, Synapses s) 
        
    
    let private updateLayer (Layer layer) manupulation =
        match manupulation with
        | UpdateNeuron (neuron, m) -> 
            let copy = Array.copy layer
            copy.[neuron] <- updateNeuron copy.[neuron] m
            Layer copy
        
    let update manupulation (Net net)  =
        match manupulation with
        | UpdateLayer (layer, m) ->
            let copy = Array.copy net
            copy.[layer] <- updateLayer copy.[layer] m
            Net copy
            
            
   
