module Walkers

open MathNet.Numerics.Random
open System

type Range = float * float

let randForRange a b =
    let unif = SystemRandomSource.Default.NextDouble()
    a + (unif * (b - a))

type State = {
    Acc: float
    Speed: float
    Pos: float
    Delay: int
}

let makeWalker (accelA, accelB) (speedA, speedB) (posA, posB) (updateEvery: int) =
    let bounce (curr: State) =
        let nextPos = curr.Pos + curr.Speed
        let nextSpeed =
            if nextPos > posB || nextPos < posA then
                -1. * curr.Speed
            else
                Math.Clamp(curr.Speed + curr.Acc, speedA, speedB)
        Math.Clamp(nextPos, posA, posB), nextSpeed
    let midPos = (posA + posB) / 2.
    let shy s =
        let acc =
            if s.Speed < 0. && s.Acc < 0. && s.Pos < midPos then
                s.Acc * (s.Pos - posA) / (posB - posA)
            else if s.Speed > 0. && s.Acc > 0. && s.Pos > midPos then
                s.Acc * (posB - s.Pos) / (posB - posA)
            else
                s.Acc
        {
            s with Acc = acc
        }
    let step (curr: State) =
        assert (curr.Delay >= 0)
        let nextPos, nextSpeed = bounce curr
        let nextAccel, nextDelay =
            if curr.Delay > 0 then
                curr.Acc, curr.Delay - 1
            else
                let newAcc = randForRange accelA accelB
                newAcc, updateEvery
        let nextState = {
            Acc = nextAccel
            Speed = nextSpeed
            Pos = nextPos
            Delay = nextDelay
        }
        shy nextState
    step

let demo nReps =
    // a is more crazy because of greater acceleration range and shorter update interval
    let a = makeWalker (-0.4, 0.4) (-5., 5.) (-100., -1.) 10
    let b = makeWalker (-0.2, 0.2) (-5., 5.) (-100., -1.) 100
    let initialState = {
        Acc = 0.0
        Speed = 0.0
        Pos = -1.0
        Delay = 10
    }
    printfn "%s" "walker,step,acc,speed,pos"
    let rec repeat aState bState curr n =
        if curr > n then
            ()
        else
            printfn "a,%d,%f,%f,%f" curr aState.Acc aState.Speed aState.Pos
            printfn "b,%d,%f,%f,%f" curr bState.Acc bState.Speed bState.Pos
            repeat (a aState) (b bState) (curr + 1) n
    repeat initialState initialState 1 nReps