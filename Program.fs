//Example of representing an illegal state
//with some code to generate MPAN's

open System

let private ProfileClasses =
    [
        00
        01
        02
        03
        04
        05
        06
        07
        08
        09
    ] |> Set.ofList

let private Distributors =
    [
        0
        1
        2
        3
        4
        5
        6
        7
        8
        9
    ] |> Set.ofList

type ProfileClass(value : int) =
    do
        if ProfileClasses |> Set.contains value |> not then
            raise (Exception("Invalid ProfileClass"))
    override __.ToString() = sprintf "%02i" value

type MeterTimeSwitchClass(value : int) =
    do
        if value < 1 || value > 999 then
            raise (Exception("Invalid MeterTimeswitchClass"))
    override __.ToString() = sprintf "%03i" value

type LineLossFactorClass(value : int) =
    do
        if value < 1 || value> 999 then
            raise (Exception("Invalid LineLossFactorClass"))
    override __.ToString() = sprintf "%03i" value

type DistributorId(value : int) =
    do
        if Distributors |> Set.contains value |> not then
            raise (Exception("Invalid DistributorID"))
    override __.ToString() = sprintf "%02i" value

type UniqueIdentifier(value : int64) =
    do
        if value < 1L || value > 9999999L then
            raise (Exception("Invalid UniqueIdentifier"))
    override __.ToString() = sprintf "%010i" value

type CheckDigit(value : int) =
    do
        if value < 0 || value > 9 then
            raise (Exception("Invalid CheckSum"))
    override __.ToString() = sprintf "%i" value
    member __.Value = value

let validationPrimes = [3;5;7;13;17;19;23;29;31;37;41;43]

type MPAN 
    (
        profileClass : ProfileClass,
        meterTimeswitchClass : MeterTimeSwitchClass,
        lineLossFactor : LineLossFactorClass,
        distributorId : DistributorId,
        uniqueIdentifier : UniqueIdentifier,
        checkDigit : CheckDigit
    ) = 
    let valid (candidate: string) =
        let len = candidate.Length
        let body = candidate.ToString().Substring(len-1, 1) |> Int32.Parse
        let total =
            distributorId.ToString() + uniqueIdentifier.ToString()
            |> Seq.zip validationPrimes
            |> Seq.sumBy (fun (p, c) ->
               let n = Int32.Parse(c.ToString())
               n * p)
        let checkSumCalc = total % 11 % 10
        checkSumCalc = checkDigit.Value

    let testData = 
        let rand = new Random()
        Array.init 1000000 (fun _ ->
            let r = rand.Next()
            sprintf "%012i" r)

    let validSerial() =
        let good, bad =
            testData
            |> Array.partition (fun s -> valid s)
        good.Length, bad.Length

    let validParallel() =
        let good, bad =
            testData
            |> Array.Parallel.partition (fun s -> valid s)
        good.Length, bad.Length 

    

    
(*
    do
        if not valid then
            raise (Exception("CheckSum Error"))
    member this.ProfileClass = profileClass.ToString()
    member this.MeterTimeSwitchClass = meterTimeswitchClass.ToString()
    member this.LineLossFactorClass = lineLossFactor.ToString()
    member this.DistributorId = distributorId.ToString()
    member this.UniqueIdentifier = uniqueIdentifier.ToString()
    member this.CheckDigit = checkDigit.ToString()

let mpan = MPAN(ProfileClass(1), MeterTimeSwitchClass(801), LineLossFactorClass(100),
               DistributorId(20), UniqueIdentifier(0001636844L), CheckDigit(1))

*)



[<EntryPoint>]
let main argv = 
    printfn "Bite me" 
    0 // return an integer exit code


