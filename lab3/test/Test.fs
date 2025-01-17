module Test 

open Xunit
open System
open LinearInterpolation
open NewtonInterpolation

let isEqual tolerance part1 part2 =
    Seq.forall2 (fun (_, value1) (_, value2) -> 
        Math.Abs(float value1 - float value2) < tolerance) 
        part1 part2

[<Fact>]
let ``Test Linear Interpolation with two points`` () =
    let actual = linearInterpolation (0.0, 0.0) (1.571, 1.0) 1.0
    let expected = seq { (0.0, 0.0); (1.0, 0.6365372374); (2.0, 1.273074475) }
    
    Assert.True(isEqual 0.01 expected actual)

[<Fact>]
let ``Test Linear Interpolation with negative step`` () =
    let actual = linearInterpolation (0.0, 0.0) (1.571, 1.0) (-1.0)
    let expected = seq { (0.0, 0.0); (-1.0, -0.6365372374); (-2.0, -1.273074475) }
    
    Assert.True(isEqual 0.01 expected actual)

[<Fact>]
let ``Test Newton Interpolation for multiple points`` () =
    let points = [ (0.0, 0.0); (1.0, 2.0); (4.0, 5.0); (6.0, 9.0) ]
    let actual = newtonInterpolation points 1.0
    let expected = seq {
        (0.0, 0.0);
        (1.0, 2.0);
        (2.0, 3.2);
        (3.0, 4.05);
        (4.0, 5.0);
        (5.0, 6.5);
        (6.0, 9.0)
    }

    Assert.True(isEqual 0.01 expected actual)

[<Fact>]
let ``Test Newton Interpolation with negative values`` () =
    let points = [ (-3.0, 9.0); (-2.0, 4.0); (-1.0, 1.0); (0.0, 0.0) ]
    let actual = newtonInterpolation points 1.0
    let expected = seq {
        (-3.0, 9.0);
        (-2.0, 4.0);
        (-1.0, 1.0);
        (0.0, 0.0)
    }

    Assert.True(isEqual 0.01 expected actual)

[<Fact>]
let ``Test Newton Interpolation with small step`` () =
    let points = [ (0.0, 0.0); (1.0, 1.0); (2.0, 4.0); (3.0, 9.0); (4.0, 16.0) ]
    let actual = newtonInterpolation points 0.5
    let expected = seq {
        (0.0, 0.0);
        (0.5, 0.25);
        (1.0, 1.0);
        (1.5, 2.25);
        (2.0, 4.0);
        (2.5, 6.25);
        (3.0, 9.0);
        (3.5, 12.25);
        (4.0, 16.0)
    }

    Assert.True(isEqual 0.01 expected actual)

