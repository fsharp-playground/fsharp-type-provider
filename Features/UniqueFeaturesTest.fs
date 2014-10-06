namespace UniqueFeaturesTest




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
