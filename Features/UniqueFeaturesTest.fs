namespace UniqueFeaturesTest


module ThrowException = 

module WorkWithException =

    open System
    open NUnit.Framework
    open FsUnit

    let exceptionCatch() =
        try 
            let a = 3 / 0
            "Ok"
        with
        | :? DivideByZeroException -> "Error"

    [<TestFixture>]
    type ExceptionTest() =
         [<Test>]
         member this.TryCatch() =
             exceptionCatch() |> should equal "Error"

module ParameterizedActivePattern =
    open NUnit.Framework
    open FsUnit
    open System.Text.RegularExpressions

    let (|RegexMatch|_|) (pattern:string) (input:string) =
        let regex = Regex(pattern).Match(input)
        if regex.Success then Some(List.tail [for x in regex.Groups -> x.Value])
        else None

    let parsePhoneNumber str =
        match str with
        | RegexMatch "(\d{3})-(\d{3})-(\d{4})" out -> System.String.Join("", out);
        | RegexMatch "(\d{3})-(\d{3})(\d{4})" out -> System.String.Join("", out);
        | _ -> "NotSupportedFormat"

    let (|Divisible|_|) x y =
        if y % x = 0 then Some Divisible else None

    let f2 = function
        | Divisible 2 & Divisible 3 -> "DivisibleBy6"
        | _ -> "Other"


    [<TestFixture>]
    type TestParameterize() =
        [<Test>]
        member this.TestDivisible() =
            f2 6 |> should equal "DivisibleBy6"
            f2 5 |> should equal "Other"

        [<Test>]
        member this.TestRegex() =
            parsePhoneNumber "000-111-2222" |> should equal "0001112222"
            parsePhoneNumber "000-1112222" |> should equal "0001112222"
            parsePhoneNumber "0001112222" |> should equal "NotSupportedFormat"


module MulticasePattern =
    open NUnit.Framework
    open FsUnit

    let (|FirstQuarter|SecondQuarter|ThirdQuarter|FourthQuarter|) (date: System.DateTime)  =
            let month = date.Month
            match month with
            | 1 | 2 | 3 -> FirstQuarter month
            | 4 | 5 | 6 -> SecondQuarter month
            | 7 | 8 | 9 -> ThirdQuarter month
            | _ -> FourthQuarter month

    let newYearRes date =
        match date with
        | FirstQuarter _ -> sprintf "HiNewYear"
        | SecondQuarter _ -> sprintf "SummerBBQ"
        | ThirdQuarter _ -> sprintf "Diet"
        | FourthQuarter _ -> sprintf "Apple"


    [<TestFixture>]
    type MulticaseTest() =
        [<Test>]
        member this.TestMulticase() =
            newYearRes (new System.DateTime(2014,1,1)) |> should equal "HiNewYear"
            newYearRes (new System.DateTime(2014,5,1)) |> should equal "SummerBBQ"
        

module PartialCaseActivePattern = 
    open NUnit.Framework
    open FsUnit

    let (|LessThan10|_|) x = if x < 10 then Some x else None
    let (|Btw10And20|_|) x = if x >= 10 && x < 20 then Some x else None

    let checkNumber2 x =
        match x with
        | LessThan10 a -> sprintf "%A LessThan10" a
        | Btw10And20 a -> sprintf "%A Between10And20" a
        | _ -> "BigNumber"

    [<TestFixture>]
    type PartialTest() =
        [<Test>]
        member this.TestPatialPattern() =
            checkNumber2 100 |> should equal "BigNumber"
            checkNumber2 15 |> should equal "15 Between10And20"

module SingleActivePattern =

    open NUnit.Framework
    open FsUnit

    let (| Remainder |) x = x % 2

    [<TestFixture>]
    type RemainderTest() =
        [<Test>]
        member this.Process() =
            let checkNumber x = 
                match x with
                | Remainder 0 -> "Even"
                | Remainder 1 -> "Odd"
                | _ -> "No"

            checkNumber 5 |> should equal "Odd"
            checkNumber 6 |> should equal "Even"

module TypePattern =


    open NUnit.Framework
    open FsUnit

    let checkTest(x:obj) =
        match x with
        | :? int as i -> "Integer"
        | :? float as f -> "Float"
        | :? string as s -> "String"
        | _ -> "Fake"

    [<TestFixture>]
    type TypeTtest() =
        [<Test>]
        member this.CheckType() =
            checkTest "Hello" |> should equal "String"
            checkTest 100 |> should equal "Integer"
            checkTest 100. |> should equal "Float"


module AndOrVarPattern =

    open NUnit.Framework
    open FsUnit

    let point2 point =
        match point with
        | x,y & 0, 0 -> "original"
        | x,y & 0, _ -> "x axis"
        | x,y & _, 0 -> "y axis"
        | _ -> "other"

    let testPoint tuple =
        match tuple with
        | x, y when x = y -> "OnTheLine"
        | x, y when x > y -> "BelowTheLine"
        | _ -> "UpToLine"

    [<TestFixture>]
    type AndOrTest() =
        [<Test>]
        member this.TestVar() =
            testPoint (100,100) |> should equal "OnTheLine"
            testPoint (100, 0) |> should equal "BelowTheLine"

        [<Test>]
        member this.TestPoint() =
            point2 (0,0,0) |> should equal "original"
            point2 (1,100, 0) |> should equal "y axis"


module ListAndArrayPattern =
    open NUnit.Framework
    open FsUnit

    let arrayLength array =
        match array with
        | [||] -> 0
        | [|_|] -> 1
        | [|_;_|] -> 2
        | [|_;_;_|] -> 3
        | _ -> Array.length array

    [<TestFixture>]
    type ArrayTest() =

        [<Test>]
        member this.TestPatternMatchingForList() =
            let list = [1;2;3;4;]
            match list with 
            | h0::h1::t ->  sprintf "two element %A %A and tail %A" h0 h1 t
            | _ -> System.String.Empty
            |> should startWith "two"

            let objList = [box(1); box("a"); box('c')]
            match objList with
            | [:? int as i; :? string as str;:? char as ch] -> sprintf "values are %A %A %A" i str ch
            | _ -> System.String.Empty
            |> should startWith "values"

        [<Test>]
        member this.TestCon() = 
            let rec listLength list =
                match list with 
                | head :: tail -> 1 + (listLength tail)
                | [] -> 0

            listLength [1;2;3;] |> should equal 3
            listLength [] |> should equal 0

        [<Test>]
        member this.TestLength() = 
            arrayLength [||] |> should equal 0
            arrayLength [|1;2;3|] |> should equal 3

(* matching *)
module PatternMatching =

    open NUnit.Framework
    open FsUnit

    let isEvenNumber x = x % 2 = 0
    let transformEvenToZero x =
        match isEvenNumber x with
        | true -> 0
        | false -> x

    [<TestFixture>]
    type PatternTest() =

        [<Test>]
        member this.TestDict() =
            let f x =
                let dict = System.Collections.Generic.Dictionary()
                dict.[0] <- "Zero"
                dict.[1] <- "One"

                match dict.TryGetValue x with
                | true, v -> sprintf "found %A mapped to %s" x v
                | false, _ -> "cannot find"

            f 0 |> should equal "found 0 mapped to Zero"
            f 5 |> should equal "cannot find"

        [<Test>]
        member this.TestTuplePattern() =
            let sample (x,y) = 
                match x,y with
                | true, true -> true
                | false, false -> true 
                | _ -> false

            sample (true, false) |> should equal false
            sample (false, true) |> should equal false
            sample (false, false) |> should equal true
            sample (true, true) |> should equal true

        [<Test>]
        member this.TestMatch() =
            transformEvenToZero 2 |> should equal 0
            transformEvenToZero 1 |> should equal 1

        [<Test>]
        member this.TestFunctionKeyword() =
            let sample x = 
                match x with
                | 1 -> "One"
                | 2 -> "Two"
                | _ -> "None"

            sample 1 |> should equal "One"
            sample 9 |> should equal "None"

(* Comparition operator *)
module CompareOperation =

    open NUnit.Framework
    open FsUnit

    type Record = { x:int ; y:int }
    type Tuple = int * int
    type DU = 
        | Case1 of int
        | Case2 of string
        | Case3 of float

    [<TestFixture>]
    type CompareTest() =
        [<Test>]
        member this.CompareTuple() =
            (1, 1) = (2, 1) |> should equal false
            (1+1, 1) = (2, 1) |> should equal true

        [<Test>]
        member this.CompareRecord() =
            { x = 100; y = 0 } = { x = 100; y = 0} |> should equal true

        [<Test>]
        member this.CompareTupleType() =
            let x: Tuple = (1, 100)
            let y: Tuple = (1, 200)
            let z: Tuple = (1, 300)
            compare x y |> should equal -1
            compare z y |> should equal 1
            compare x x |> should equal 0

        [<Test>]
        member this.CompareDU() =
            compare (Case1 100) (Case1 200) |> should equal -1
            compare (Case2 "c") (Case2 "b") |> should equal 1
            compare (Case3 2.) (Case3 2.) |> should equal 0

(* Binary tree BU *)
module BinaryTreeBU = 

    open NUnit.Framework
    open FsUnit

    type NodeType = int
    type BinaryTree =
        | Nil
        | Node of NodeType * BinaryTree * BinaryTree

    let tree = 
        Node(5,
            Node(42,
                Node(3, Nil, Nil), 
                Node(2, Nil, Nil)),
            Node(4, 
                Node(13, 
                    Node(14, Nil, Nil),
                    Node(16, Nil, Nil)),
                Node(12,
                    Node(15, Nil, Nil),
                    Node(21, 
                        Node(22, Nil, Nil), 
                        Nil))))


    let layerTreverse state =
        match state with 
        | [] -> None
        | h::t ->
            match h with
            | Nil -> None
            | Node(v, Nil, Nil) -> Some(v, t)
            | Node(v, subTree, Nil)
            | Node(v, Nil, subTree) -> Some(v, t@[subTree])
            | Node(v, l, r) -> Some(v, t@[l;r] )


    [<TestFixture>]
    type TreverseTest() =
        [<Test>]
        member this.Test() =
            let x = 
                [tree]
                |> Seq.unfold layerTreverse
                |> Seq.toList

            x.Head |> should equal 5 
            x.Tail.Head |> should equal 42
            printfn "%A" x


(* DU with interface *)
module DUWithInterface =

    open NUnit.Framework
    open FsUnit

    type IShape =
        abstract Area: double with get

    type ShapeWithInterface = 
        | Circle of double
        | Triangle of double * double * double
        | Rectangle of double * double
        interface IShape
            with member this.Area = 
                    match this with 
                    | Circle(r) -> r * r * System.Math.PI
                    | Triangle(a, b, c) -> 
                        let  s = (a + b + c) / 2.
                        sqrt(s * (s - a) * (s - b) * (s - c ))
                    | Rectangle(a, b) -> a * b
        override this.ToString() = "ThisIsAShapeWithInterfaceType"

    [<TestFixture>]
    type IntefaceTest() =
        [<Test>]
        member this.Test() =
            let x = Rectangle(10., 10.)
            (x :> IShape).Area |> should equal 100

(* DU with method *)
module DUWithMethod =
    type Tree =
        | Tip of NodeType
        | Node of int * Tree * Tree
        override this.ToString() = "ThisIsATreeDU"

    and NodeType =
        | NodeContent of int
        override this.ToString() = "ThisIsANodeTypeDU"


module RecuriveDU = 
    type Tree =
        | Tip of NodeType
        | Node of int * Tree * Tree
    and NodeType =
        | NodeContent of int


module DefineDiscrimentalUnion = 
    type Shape =
        | Circle of double
        | Triangle of double * double * double
        | Rectangle of double * double

    type Number = OddNumber | EventNumber


module DefaultRecordConstructor = 

    open NUnit.Framework
    open FsUnit

    [<CLIMutable>]
    type R1 = { x: int; y: int }

    type R2 = {
        x: int
        y: int }

    [<TestFixture>]
    type ConstructAttribute() =
        [<Test>]
        member this.Construct() =
            //let r1 = new R1(100, 200) // error no constructor enable
            let r2 = { R2.x = 100; R2.y = 200 }
            //r1.x |> should equal 100
            r2.x |> should equal 100

module RecordComparition = 

    open NUnit.Framework
    open FsUnit

    type Point = {
        x:float
        mutable y:float }

    [<TestFixture>]
    type Comparision() =
        [<Test>]
        member this.Compare() =
            let p = { x = 100.0; y = 100.0}
            let q = { x = 100.0; y = 100.0}
            p = q |> should equal true

            q.y <- 200.0
            p = q |> should equal false

module StaticMember =

    open NUnit.Framework
    open FsUnit

    type Point2D = 
        {
            x: float
            y: float
        }
        static member OriginalPoint = { x = 100.; y = 100. }

    [<TestFixture>]   
    type OriginalPointTest() =
        [<Test>]
        member this.StaticTest() =
            Point2D.OriginalPoint.x |> should equal 100.



(* Mutate Record *)
module MutableRecord =

    open NUnit.Framework
    open FsUnit
    
    type Car = 

        {
            Make: string
            Model: string
            mutable Odemeter: int 
        }

        member this.Drive(mile) =
            this.Odemeter <- this.Odemeter + mile

        member this.CurrentMileage with get() = this.Odemeter

    [<TestFixture>]
    type MutateTest() =
        [<Test>]
        member this.Drive() =
            let car = {
                Make = "Plymouth"
                Model = "Neon"
                Odemeter = 100 }

            car.Drive 200 
            car.CurrentMileage |> should equal 300
    

module CopyRecord = 

    open NUnit.Framework
    open FsUnit

    type Person = {
        First: string
        Last: string }

    [<TestFixture>]
    type CopyTest() =
        [<Test>]
        member this.Copy() =
            let person = {
                First = "wk"
                Last = "kw" }
            let copy = { person with First = "new" }
            copy.First |> should equal "new"
            copy.Last |> should equal "kw"


module DefineRecord = 

    type Point2D = 
        { x : float
          y : float }
    
    type Point = 
        { x : float
          y : float }
    
    let p2d = 
        { x = 100.
          y = 100. }

    let p = {
        Point.x = 100.
        Point.y = 100. }


module MeasureAndNumber = 
    open NUnit.Framework
    open FsUnit
    
    [<Measure>]
    type lb
    
    let removeUnit<[<Measure>] 'u> x = float x
    let giveUnit (x : float) = LanguagePrimitives.FloatWithMeasure x
    
    [<TestFixture>]
    type ConvertType() = 
        [<Test>]
        member this.ToFloat() = 
            removeUnit 100.<lb> |> should equal 100.
            giveUnit 100. |> should equal 100.<lb>
            Assert.AreEqual(giveUnit 100., 100.<lb>)

(* Generic measure *)
module GenericMeasure = 
    open NUnit.Framework
    open FsUnit
    
    [<Measure>]
    type lb
    
    [<Measure>]
    type inc
    
    let add (x : float<'u>) (y : float<'u>) = x + y * 2.
    
    [<TestFixture>]
    type GenericMeasureTest() = 
        [<Test>]
        member this.AddTest() = 
            add 100.<lb> 200.<lb> |> should equal (100. + 200. * 2.)
            add 100.<inc> 200.<inc> |> should equal (100. + 200. * 2.)

(* Static measure *)
module StaticMesureMember = 
    open NUnit.Framework
    open FsUnit
    
    [<Measure>]
    type m = 
        static member C = 3.28<foot/m>
        static member FromFoot(x : float<foot>) = x / m.C
        static member ToFoot(x : float<m>) = x * m.C
    
    and [<Measure>] foot = 
        static member FromMeter = m.ToFoot
        static member ToMeter = m.FromFoot
    
    [<TestFixture>]
    type StaticMeasureTest() = 
        
        [<Test>]
        member this.ConvertFootToMeter() = foot.FromMeter 2.<m> |> should equal (2. * 3.28)
        
        [<Test>]
        member this.ConvertMeterToFoot() = foot.ToMeter 5.<foot> |> should equal (5. / 3.28)

(* Convert measure *)
module ConvertMeature = 
    open NUnit.Framework
    open FsUnit
    
    [<Measure>]
    type g
    
    [<Measure>]
    type kg
    
    let convertGramToKilogram (x : float<g>) = x / 1000.0<g/kg>
    
    type ConvertTest() = 
        member this.TestConvert() = 
            convertGramToKilogram 2200.0<g> |> should equal 2.2<kg>
            Assert.AreEqual(convertGramToKilogram 2200.0<g>, 2.2<kg>)

(* Unit of measure *)
module UnitOfMeasure = 
    open NUnit.Framework
    open FsUnit
    
    [<Measure>]
    type cm
    
    [<Measure>]
    type ml = cm^3
    
    let x = 3.<cm>
    let c = x * x * x
    
    [<TestFixture>]
    type MeasureTest() = 
        [<Test>]
        member this.TestCm() = 
            c |> should equal 27.<ml>
            c |> should equal 27.<cm>

(* Convert null *)
module ConvertNull = 
    open NUnit.Framework
    open FsUnit
    
    type A() = 
        let mutable a : option<A> = None
        
        member this.Value 
            with get () = 
                match a with
                | Some n -> n
                | None -> Unchecked.defaultof<A>
            and set (v) = 
                if v = Unchecked.defaultof<A> then a <- None
                else a <- Some v
    
    [<TestFixture>]
    type ConvertNullTest() = 
        [<Test>]
        member this.TestConvert() = 
            A().Value |> should equal None
            let a = A()
            a.Value <- A()
            a.Value |> should not' (be Null)

(* Nullable Type *)
module Nullable = 
    open NUnit.Framework
    open FsUnit
    
    [<AllowNullLiteral>]
    type NullableType() = 
        static member TestNullable(condition) : NullableType = 
            if condition then NullableType()
            else null
    
    [<TestFixture>]
    type NullTest() = 
        [<Test>]
        member this.TestNull() = 
            NullableType.TestNullable true |> should not' (be Null)
            NullableType.TestNullable false |> should be Null

(* Option Type *)
module WorkWithOption = 
    open NUnit.Framework
    open FsUnit
    
    let filterOutOddNumber a = 
        if a % 2 = 0 then Some(a)
        else None
    
    [<TestFixture>]
    type OptionTest() = 
        
        [<Test>]
        member this.TestMod() = 
            filterOutOddNumber 5 |> should equal None
            filterOutOddNumber 2 |> should equal (Some 2)
        
        [<Test>]
        member this.TestString() = 
            let check s1 = 
                match s1 with
                | Some data -> "Good"
                | None -> "Bad"
            check (Some "DD") |> should equal "Good"
            check None |> should equal "Bad"

(* I don't have idea how to test this module. *)
module WPFCommand = 
    open System.Windows.Input
    open NUnit.Framework
    open FsUnit
    
    let createCommand action canExecute = 
        let event1 = Event<_, _>()
        { new ICommand with
              member this.CanExecute(obj) = canExecute (obj)
              member this.Execute(obj) = action (obj)
              member this.add_CanExecuteChanged (handler) = event1.Publish.AddHandler(handler)
              member this.remove_CanExecuteChanged (handler) = event1.Publish.RemoveHandler(handler) }
    
    let myDummyCommand = createCommand (fun _ -> ()) (fun _ -> true)
    
    [<TestFixture>]
    type Test() = 
        [<Test>]
        member this.TestCommand() = myDummyCommand.Execute()

(* Object Expression with Property *)
module ObjectExpressionWithProperty = 
    open NUnit.Framework
    open FsUnit
    
    type IMyInterface = 
        abstract F : unit -> string
        abstract Prop0 : string with get, set
        abstract Prop1 : string with get, set
    
    let myData = ref ""
    
    let objectExpression = 
        let x = ref ""
        { new IMyInterface with
              member this.F() = "HelloObjectExpression"
              
              member this.Prop0 
                  with get () = !x
                  and set (v) = x := v
              
              member this.Prop1 
                  with get () = !myData
                  and set (v) = myData := v }
    
    let objectExpressionExtendObject = 
        { new System.Object() with
              member this.ToString() = "HelloFromObjectExpression" }
    
    [<TestFixture>]
    type Test() = 
        
        [<Test>]
        member this.TestObjectExpression() = 
            objectExpression.F() |> should equal "HelloObjectExpression"
            objectExpression.Prop0 |> should equal ""
            objectExpression.Prop0 <- "Hello"
            objectExpression.Prop0 |> should equal "Hello"
        
        [<Test>]
        member this.TestExtendObject() = 
            objectExpressionExtendObject.ToString() |> should equal "HelloFromObjectExpression"

(* Object Expressions *)
module ObjectExpressions = 
    open NUnit.Framework
    open FsUnit
    
    type IA = 
        abstract F : int with get, set
    
    type IMyInterfaceA = 
        abstract FA : unit -> string
    
    type IMyInterfaceB = 
        abstract FB : unit -> string
    
    [<TestFixture>]
    type ObjectExpressionsTest() = 
        
        [<Test>]
        member this.TestImplementMultipleInterfaces() = 
            let implA = 
                { new IMyInterfaceA with
                      member this.FA() = "HelloFA" }
            
            let implM = 
                { new IMyInterfaceA with
                      member this.FA() = "HelloFA"
                  interface IMyInterfaceB with
                      member x.FB() = "HelloFB" }
            
            implA.FA() |> should equal "HelloFA"
            implM.FA() |> should equal "HelloFA"
        
        // implM.FB() |> should equal "HelloFB"
        [<Test>]
        member this.TestImplementInterface() = 
            let objectExpression = 
                let storage = ref 9
                { new IA with
                      
                      member m.F 
                          with get () = !storage
                          and set (v) = storage := v }
            objectExpression.F |> should equal 9
            objectExpression.F <- 100
            objectExpression.F |> should equal 100

(* Reference Cell *)
module ReferenceCell = 
    open NUnit.Framework
    open FsUnit
    
    [<TestFixture>]
    type ReferenceCellTest() = 
        [<Test>]
        member this.TestChangeValue() = 
            let refVar = ref 1
            refVar := 2
            !refVar |> should equal 2
