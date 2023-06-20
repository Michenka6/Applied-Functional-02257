open System

// Define the ODE function
let odeFunction (t: float) (y: float) = -0.5 * y

let t0 = 0.0
let y0 = 1.0
let h = 0.1
let n = 10

// Euler method for numerical integration (immutable version)
let eulerMethod (f: float -> float -> float) (t0: float) (y0: float) (h: float) (n: int) =
    let rec eulerIter t y acc count =
        if count = 0 then
            acc
        else
            let tNext = t + h
            let newY = y + h * f t y
            eulerIter tNext newY (newY :: acc) (count - 1)

    eulerIter t0 y0 [ y0 ] n

// Trapezoidal method for numerical integration (immutable version)
let trapezoidalMethod (f: float -> float -> float) (t0: float) (y0: float) (h: float) (n: int) =
    let rec trapezoidalIter t y acc count =
        if count = 0 then
            acc
        else
            let tNext = t + h
            let yPredict = y + h * f t y
            let yCorrect = y + 0.5 * h * (f t y + f tNext yPredict)
            trapezoidalIter tNext yCorrect (yCorrect :: acc) (count - 1)

    trapezoidalIter t0 y0 [ y0 ] n

// Solve the ODE using the Euler method
let eulerSolution = eulerMethod odeFunction t0 y0 h n

// Solve the ODE using the Trapezoidal method
let trapezoidalSolution = trapezoidalMethod odeFunction t0 y0 h n

printfn "Euler Method Solution: %A" eulerSolution
printfn "Trapezoidal Method Solution: %A" trapezoidalSolution
