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
    let step (curr: State) =
        assert (curr.Delay >= 0)
        let nextSpeed = Math.Clamp(curr.Speed + curr.Acc, speedA, speedB)
        let nextPos = Math.Clamp(curr.Pos + curr.Speed, posA, posB)
        let nextAccel, nextDelay =
            if curr.Delay > 0 then
                curr.Acc, curr.Delay - 1
            else
                randForRange accelA accelB, updateEvery
        {
            Acc = nextAccel
            Speed = nextSpeed
            Pos = nextPos
            Delay = nextDelay
        }
    step

let demo nReps =
    let a = makeWalker (-2., 2.) (-5., 5.) (-100., -1.) 10
    let b = makeWalker (-2., 2.) (-5., 5.) (-100., -1.) 10
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