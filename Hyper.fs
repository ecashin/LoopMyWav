module Hyper

open SharpLearning.Optimization


let parameters (sampleRate: uint32) : IParameterSpec [] =
    let sr = sampleRate |> float
    [|
        // max delay
        MinMaxParameterSpec(
            1.0,
            (sr * 3.0),
            Transform.Linear,
            ParameterType.Discrete
        );
        // min delay will be -1
        // max abs acceleration
        MinMaxParameterSpec(
            0.001,
            sr / 1000.0,
            Transform.Linear,
            ParameterType.Continuous
        );
        // max abs speed
        MinMaxParameterSpec(
            0.001,
            sr / 1000.0,
            Transform.Linear,
            ParameterType.Continuous
        );
        // update delay
        MinMaxParameterSpec(
            1.0,
            sr * 5.0,
            Transform.Linear,
            ParameterType.Discrete
        )
    |]

let makeOpt parms iters =
    BayesianOptimizer(parms, iters)

let makeWalkerDef (parms: float []) : Walkers.WalkerDef =
    {
        MostDelay = parms.[0] |> int
        LeastDelay = 1
        AccA = -parms.[1]
        AccB = parms.[1]
        SpeedA = -parms.[2]
        SpeedB = parms.[2]
        UpdateDelay = parms.[3] |> int
    }