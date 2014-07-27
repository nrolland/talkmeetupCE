#I @"./packages"
#r @"FSharpx.Core.1.8.41/lib/40/FSharpx.Core.dll"

module DSL = 
    //Full language
    type expr = 
       | CstI of int
       | CstB of bool
       | Prim of string * expr * expr

    let exp1 = Prim("*", Prim("+",  CstI 2 , CstI 1), CstI 4) 

    let rec eval (e:expr) :int =
        match e with 
        | CstI i -> i
        | Prim("+",e1, e2) -> eval e1 + eval e2
        | Prim("-",e1, e2) -> eval e1 - eval e2
        | Prim("*",e1, e2) -> eval e1 * eval e2
        | _ -> failwith "dont know how to evaluate"

    let val1 = eval exp1

    let expKO = Prim("*", Prim("+",  CstB true , CstI 1), CstI 4) 
    //This expression is wrong : substraction is not defined on Booleans !
    //- No type system in the target language : we can write stupid expressions
    //- Our target language is made of "EXPR" only.. we dont reuse F# 

    //building AST, type checking, evaluating, is done by us

module ShallowEmbedding = 
   
    //we represent the fundamental type of our domain in F# 
   type CstI = CstI of int
   type CstB = CstB of bool

    //We build primitive blocks - in valid F#
   let plus (CstI a) (CstI b) = CstI (a + b)
   let times (CstI a) (CstI b) = CstI (a * b)
   let ifbranch (CstB v) a b = if v then a else b

   let exp1  = times (plus (CstI 2)(CstI 1)) (CstI 4) 
   //let expKO = times (plus (CstB true)(CstI 1)) (CstI 4) 

module ShallowEmbedding2 = 
    //a hardware simulator - a signal is a stream of input data
    //each component takes some signal in and create some signal out

    //we represent the fundamental type of our domain in F# 
    type Signal<'a> = 'a seq //a signal is a sequence of value, one for each clock cycle

    //We build primitive blocks - in valid F#
    let low :Signal<bool>       = Seq.initInfinite (fun _ -> false) //always false
    //a multiplexer 
    let mux (si:Signal<bool>)(sa:Signal<'a>,sb:Signal<'a>):Signal<'a> = 
        Seq.zip3 si sa sb |> Seq.map (fun (i,a,b) -> if i then a else b)
    // a register delays a signal
    let reg init (si:Signal<'a>) :Signal<'a>= seq{ yield init; yield! si} 
    //add 
    let plusone (si:Signal<int>) :Signal<int>= Seq.map ((+) 1) si

    //Everything which interests us is built from those primitive blocks using the primitive type
    //And they are ALSO a valid F# function, so we have a type system : F# type system !
    //Program made for our subset will be verified for free.

    //counts the number of true
    let testSignal = seq{while true do yield! [true;false;false]}
    let counter (si:Signal<bool>) = //si is Signal<bool> not int
        let rec loop  = let d = delay()
                        mux si (d |> plusone, d) 
        and     delay() = seq{ yield 0; yield! loop}
        loop
    let testCountOK = counter testSignal |> Seq.take 5 |> Seq.toArray

    //Summary : if the primitive types capture our domain, we have a free type system
    //(provided the binding and evaluation in our domain is like F#...) 
    let counterKO (si:Signal<bool>) =  
        let rec loop  = let d = delay()
                        mux si (d |> plusone, d) 
        and     delay() = reg 0 loop //eager evaluation breaks functional abstraction for co-inductive types
        loop                         
    let testCountKO = counterKO testSignal |> Seq.take 5 |> Seq.toArray


 

module DeepEmbedding = 
    //previously we built values. from some base signal, we built an output signal
    //with deep embedding we build expression representing a computation doing so

    //inline replaces at compile time a generic function with the appropriate concrete function
    type Return = Return with
        static member ($) (Return, t:'a option) = fun (x:'a) -> Some x
        static member ($) (Return, t:'a list)   = fun (x:'a) -> [x]
    let inline return' x : ^R = (Return $ Unchecked.defaultof< ^R> ) x
     
    type Bind = Bind with
        static member ($) (Bind, x:option<_>) = fun f -> Option.bind f x
        static member ($) (Bind, x:list<_>  ) = fun f ->
            let rec bind f = function
                             | x::xs -> f x @ (bind f xs)
                             | []    -> []
            bind f x

    let inline (>>=) x f = (Bind $ x) f

    let inline add (a:^a ) b = a + b
    let a:list<_>   = return' 1 ;;
    let a:option<_> = return' 1 ;;

     //add type class F# for + and * 
     //we can extract the AST of the expression in the target language from the expression in Fsharp language
    type expr = 
       | CstI of int
       | IfBranch of CstB * expr * expr
       | Plus of expr * expr
       | Times of expr * expr
       | Var of string
    and CstB = CstB of bool

    type TPlus = TPlus with
        static member ($) (TPlus, t : int)  = (+) t 
        static member ($) (TPlus, t : expr)  = fun (o:expr) ->  Plus(t,o)
    let inline (+) a b = (TPlus $ a) b

    type TConst = TConst with
        static member ($) (TConst, t : int)  = fun (o:int) -> o
        static member ($) (TConst, t : expr) = fun (o:int) ->  CstI(o)
    let inline return' x : ^R = (TConst $ Unchecked.defaultof< ^R> ) x

    type TTimes = TTimes with
        static member ($) (TTimes, t : int)  = (*) t
        static member ($) (TTimes, t : expr) = fun (o:expr) ->  Times(t,o)
    let inline (*) a b  = (TTimes $ a ) b


    let res : int   = return' 2 + return' 1
    let res : expr  = (return' 2 + return' 1) * return' 3
    let res : int  = (return' 2 + return' 1) * return' 3


    let inline f1 x  = (x + return' 1) * return' 3

    let ast = f1 (Var "x")
    let res = f1 2


    let rec eval (e:expr) :int =
        match e with 
        | CstI i -> i
        | Plus(e1, e2) -> eval e1 + eval e2
        | Times(e1, e2) -> eval e1 * eval e2
        | IfBranch(b1,e1, e2) -> if evalb b1 then eval e1 else eval e2
     and  evalb (e:CstB) : bool =
        match e with 
        | CstB b -> b





module ScratchPad =
    module ShallowEDSL3 = 
        //a hardware simulator - a signal is a 
        //type Signal<'a> = Next of (unit -> 'a * Signal<'a>)
        type Signal<'a> = Next of 'a * (unit -> Signal<'a>)
         
        let rec low :Signal<bool>       = Next (false, fun () -> low)
        let rec mux (Next(vi,ni):Signal<bool>)((Next(va,na)),(Next(vb,nb))) = Next((if vi then va else vb), fun () -> mux (ni()) (na(),nb()))
        let     reg init (si:Signal<'a>) :Signal<'a>= Next(init,fun () -> si)
        let rec plusone (Next(vi,ni):Signal<int>) :Signal<int>= Next(vi+1, fun () -> plusone (ni()))

        let counter (si:Signal<bool>) = 
            let rec loop()  = mux si (delay() |> plusone, delay()) 
            and     delay() = loop() |> reg 0
            loop()
        
        let rec toSeq s = seq{ let (Next(v,n)) = s
                               yield v;yield! toSeq (n())  }
        let fromSeq (s:'a seq) = 
            let a = s.GetEnumerator()
            let rec getOne () = let v = a.Current
                                a.MoveNext() |> ignore
                                Next(v, fun () -> getOne())
            getOne()

        let si = seq{while true do yield! [true;false;false]} |> fromSeq
        let t = mux si (si,si)
        let testCount = counter (fromSeq testSignal) |> toSeq |> Seq.take 5 //|> Seq.toArray
         


