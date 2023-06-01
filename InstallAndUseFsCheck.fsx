// Install and use FsCheck from a .fsx script file        Michael R. Hansen 13-09-2021
//
// You must refer to the FsCheck package before you can use it

// The following directive refers to FsCheck and should install the package
// if it is not already installed
#r "nuget: FsCheck, 2.16.03"

// or simply use:  #r "nuget: FsCheck";;
// to get the newest version

// The above ways of installing abd referencing nuget packages may be version dependent.

// You can always install FsCheck from: https://www.nuget.org/packages?q=FsCheck
// and refer using a full path to the assembly: FsCheck.dll
// as follows:                               -- revise path properly
// #r @"C:\Users\mire\.nuget\packages\fscheck\2.16.3\lib\net452\FsCheck.dll"
//
// or
// #I @"C:\Users\mire\.nuget\packages\fscheck\2.16.3\lib\net452"
// #r @"FsCheck.dll"


open FsCheck

let rec multC n ms =
    match ms with
    | [] -> []
    | m :: tail -> (m * n) :: multC n tail

let prop1 m n ks = multC (m * n) ks = multC m (multC n ks)

let t1 = FsCheck.Check.Quick prop1

let t2 = Check.Quick prop1

let t3 = Check.Verbose prop1
