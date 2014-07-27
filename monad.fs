#I @"./packages"
#r @"FSharpx.Core.1.8.41/lib/40/FSharpx.Core.dll"

//---------------------------
//I.Bottom up introduction to monad and computation expressions - 5'
module BottomUp =
    let log p = printfn "expression is %A" p

    //linear
    let loggedWorkflow =
        let x = 42
        log x
        let y = 43
        log y
        let z = x + y
        log z
        z


    //a bit functional 
    let loggedWorkflow = 
       let x = (fun x -> log x;x) 42
       let y = (fun x -> log x;x) 43
       let z = (fun x -> log x;x) (x + y)
       z

    //completely functional
    let bind(x,f) = f x
    let loggedWorkflow = 
       bind(42,(fun x -> log x;
                         bind(43,(fun y -> log y;
                                           bind(x+y, (fun z -> log z))))))
      


    //We dont want to write that spaghetti. 
    //All we have done is very mechanical, so is there a way to make it look nicer ?
    //Computation Expression
    type LoggingBuilder() =
        let log p = printfn "expression is %A" p
        member this.Bind(x,f) = log x; f x
        member this.Return(x) = x

    let logger = new LoggingBuilder()

    let loggedWorkflow = logger {  let! x = 42
                                   let! y = 43
                                   let! z = x + y
                                   return z}
    //lean and mean

//------------------
//II.Computations - 2'
module MonadAndEffect =

    // same code, different effect  
    type ReadlineBuilder() =
        member this.Bind(x,f) = System.Console.ReadLine() |> ignore; printfn "%A" x ; f x
        member this.Return(x) = x

    let readline = new ReadlineBuilder()

    let readlineWorkflow = readline {  let! x = 42
                                       let! y = 43
                                       let! z = x + y
                                       return z}


    //nicer ... for something fundamental : langage VS computation 

    //Notions of computation and monads Eugenio Moggi
    //http://www.disi.unige.it/person/MoggiE/ftp/ic91.pdf


//II.ComputationsExpression - 5'
module ComputationsExpression =
    //Computation expression VS monads : mappping all the handy constructs we have in a language.
    //exemple : Zero

    //empty branch
    let a = if true then 1 
                    else 2  //OK uniform in return type
    let a = if true then () //OK uniform because unit is the 'zero' among types (actually, the only value of type bottom, the type which is a subtype of every type)

    let ma = seq { yield! Seq.empty } 

    //For construct
    let ma = [ for i in Seq.empty do yield 1 ] //OK because Zero exists as the empty list


    //Classic F# : Pragmatic approach to leverage a CS concept 
    //here, embed the idiomatic construction that we often use inside the context of a computation
    //The F# Computation Expression Zoo -  https://research.microsoft.com/apps/pubs/default.aspx?id=217375

    //MEMBER  DESCRIPTION
    //member Delay : (unit -> M<'a>) -> M<'a>         Optional. Ensures that side effects within the monad are performed when expected.
    //member Yield : 'a -> M<'a>                      Optional. Converts the yield within the monad.
    //member For : seq<'a> * ('a -> M<'b>) -> M<'b>   Optional. Converts the for ... do ... within the monad.  M<'b> can optionally be M<unit>
    //member While : (unit -> bool) * M<'a> -> M<'a>  Optional. Converts the while ... do ... within the monad. M<'b> can optionally be M<unit>
    //member Using : 'a * ('a -> M<'b>) -> M<'b> when 'a :> IDisposable   Optional. Converts the use bindings within the monad.
    //member Combine : M<'a> -> M<'a> -> M<'a>        Optional. Used to convert sequencing within the monad. The first M<'a> can optionally be M<unit>
    //member Zero : unit -> M<'a>                     Optional. Converts the empty else branches of if/then in the monad.
    //member TryWith : M<'a> -> M<'a> -> M<'a>        Optional. Converts the try/with bindings of the monad.
    //member TryFinally : M<'a> -> M<'a> -> M<'a>     Optional. Converts the try/finally bindings of the monad.


//------------------
//III- Monadic thinking - 10'
module MonadicThinking =
// in category theory, type = base fsharp type  + composition function
// the previous example used special composition => we are always mapping normal types in some underlying 'monadic type' 

 //Summary of previous : computation type is preserved throughout

 //Id monad - the smallest spaghetti simplification
 module Id = 
    type Id<'T> = Id of 'T

    let box   (x:'a) :Id<'a> = Id x
    let unbox ((Id x):Id<'a>): 'a = x

    // the minimal type correct monad
    type IdBuilder () = 
        member this.Return (x:'a) : Id<'a> = box x
        member this.Bind(x:Id<'T>, f:'T->Id<'U>) = unbox x |> f 

    let idb = IdBuilder () 
    let result = idb    { let! a = box ("Hello,")
                          let! b = box ("World!")
                          return a + b}

    let result = idb.Bind(idb.Return("Hello,"), 
                          (fun a -> idb.Bind(idb.Return("World!"), 
                                             (fun b -> idb.Return(a + b)))))


 //Lazy monad - 5'
 module Lazy = 
    // Focus on the type T = T
    type Lazy<'T> = unit -> 'T

    let force (x:Lazy<_>) = let r = x() in printfn "using %A" r; r
    let lazy' x = fun () -> printfn "using %A" x; x  //not a good idea - why ?  -                                                   functional abstraction incurs greedy eval - greedy eval means loosing composability of evaluation strategy  

    type LazyBuilder () = 
        member this.Return (x) : Lazy<_> = fun () -> x 
        member this.Bind(x:Lazy<'T>, f:'T->Lazy<'U>) =  
            fun () -> (x |> force |> f |> force ) 
        //member this.Bind(x:Lazy<'T>, f:'T->Lazy<'U>) =  lazy' (x |> force |> f |> force )  
 
    let lazyb = LazyBuilder () 
    let result = lazyb { let! a = fun () -> ("Hello,")
                         let! b = fun () -> ("World!")
                         let! which = fun () -> true
                         if which then
                             return a
                         else return "Beurk!"}
    result() |> ignore

    lazyb.Bind(lazyb.Return("Hello,"),
          (fun a -> lazyb.Bind(lazyb.Return("World"),
                               (fun b -> lazyb.Bind(lazyb.Return(true),
                                                    (fun which -> if which then lazyb.Return a else lazyb.Return b) 
                                                   )
                               )
                              )
           ))()
         |> ignore

    lazyb.Bind(lazyb.Return("Hello,"),      
               (fun a -> lazyb.Return(a)))
         |> ignore

module Resumption = 
    type Resumption<'a> =
    | NotYet of (unit -> Resumption<'a>)
    | Result of 'a

    let step wf =
      match wf with 
      | NotYet(f) -> f()
      | Result(v) -> Result(v)

    let rec evaluate counter steps =
      printfn "[STEP: %d]" counter
      match steps with 
      | NotYet(f) -> //answer web request ...   
                     evaluate (counter + 1) (f())
      | Result(v) -> v
    
    let returnR v = Result(v)

    let rec bindR v f =
        NotYet(fun () -> match v with
                            | NotYet calcV -> bindR (calcV()) f
                            | Result value -> f value)

    type ResumptionBuilder() = 
        member x.Bind(v, f) = bindR v f
        member x.Return(v) = returnR v


    
    let resumable = new ResumptionBuilder()

    let isPrime(n) =
      let ms = int(sqrt(float(n)))
      let rec isPrimeUtil(m) =
        if m > ms then true
        elif n % m = 0 then false
        else isPrimeUtil(m + 1)
      (n > 1) && isPrimeUtil(2)


    let calculate1() = resumable {
      let numbers = [ 1000000 .. 2000000 ]
      let count = numbers |> List.filter isPrime |> List.length
      return count }

    let calculate2() = resumable {
      let numbers = [ 2000000 .. 3000000 ]
      let count = numbers |> List.filter isPrime |> List.length
      return count }
      
    let steps = resumable {  
      let! a = calculate1()
      printfn "first part finished!"
      let! b = calculate2()
      printfn "second part finished!"
      return (a, b) }


    steps |> evaluate 0  |> ignore
    let ( NotYet(f1)) = steps
    let ( NotYet(f2)) = f1()
    let ( Result(a)) = f2()
//   


//STATE monad 
 module State = 
           
    //warmup as in first paragraph      - 2'       
    let mutable counter = 0
    let DoSomething () = printfn "counter %A" counter 

    let counterWorkflow = 
        do DoSomething()
        counter <- counter + 1
        do DoSomething()
        counter <- counter + 1
        do DoSomething()
        counter <- counter + 1
     
    //Focus on the type
    type Counter<'a> = Counter of (int -> 'a * int)     
          
    let runState (Counter s) initialState = s initialState
                 
    type CounterBuilder() =
      member this.Return a = Counter (fun s -> (a, s))            
      member this.Bind(m:Counter<'a>, k:'a->Counter<'b>) =        
        Counter (fun s -> let (a,s') = runState m s 
                          runState (k a) s')                      

    let counter = new CounterBuilder()       

    //the monad cares only of the *composition* not the type itself 
    //we can/have to interact with the base type                         
    let DoSomething' counter = printfn "counter %A" counter  //almost
                               counter + 1
    let DoSomething''  = Counter (fun counter -> printfn "counter %A" counter  //correct
                                                 (), counter + 1)

    let counterWorkflow = counter {
                            do! DoSomething''
                            do! DoSomething''
                            do! DoSomething''
                        }
    runState counterWorkflow 0 |> ignore                                          
    
    //Full State monad - 10'

    //1- write the type preserved throughout the computation
    type State<'a,'s> = State of ('s -> 'a * 's)

    //-parameterized by a state (we read upstream value) 
    //-state outputed as well for reuse (we write downstream value)

    //summary 1 : We will build more complex expression, but *always of this type*

    //2- write how to use such type - out of band
    let runState (State s) initialState = s initialState


    //3- write composition rules
    // monad specifies only composition rules
    // monad <=> composition rules
    // to achieve composition, we use the concrete meaning of this monad
    // the tools are whatever it takes, the goal is *composition*
    type StateBuilder() =
      member this.Return a = State (fun s -> (a, s))                        // val returnF : 'a -> M<'a>
      member this.Bind(m:State<'a,'s>, k:'a->State<'b,'s>) =                // val bind    : M<'a> -> ('a -> M<'b>) -> M<'b>
        State (fun s -> let (a,s') = runState m s 
                        runState (k a) s')                                  // val bind    : State<'a> -> ('a -> State<'b>) -> State<'b> 
    let state = new StateBuilder()                                          // val bind   : ('s -> ('a,'s))       ->  
                                                                            //              ('a -> 's -> ('b,'s)) ->  
                                                                            //              ('s -> ('b,'s)) 

    
    //usage 1 -  specialise usage into monadic primitive - out of band
    //usage does not belong to the monad. one need to get in and out of the monad to write applicative logic
    let getState = State (fun s -> (s,s))
    let putState s = State (fun _ -> ((),s))


    //usage 1 -  compose into bigger expression and run
    let tick = state {
       let! n = getState
       do! putState (n + 1)
       let! n = getState
       do! putState (n + 1)

       let! n = getState
       return n }

    let res = runState tick 3 


    //usage 2 -  specialise usage into monadic primitive 
    let enqueue a = State (fun s -> ((), s @ [a]))
    let dequeue = State (fun (hd::tl) -> (hd, tl))


    //usage 2 -  compose into bigger expression and run
    let workflow = state {
      //let! queue = getState
      do! enqueue 4
      let! hd = dequeue
      do! enqueue (hd * 3)
      return hd }

    let res = runState workflow [3..6]



//usage 3 -  specialise usage into monadic primitive 
//open WatiN.Core
//    let openPage (url:string) = state {
//      let! (browser : Browser) = getState
//      return browser.GoTo(url) }
//
//    let enterText (textBox : string) (text : string) = state {
//      let! (browser : Browser) = getState
//      return browser.TextField(Find.ByName(textBox)).TypeText(text) }
//     
//    let containsText (text : string) = state {
//      let! (browser : Browser) = getState
//      return browser.ContainsText(text) }
//     
//    let clickButton (buttonText : string) = state {
//      let! (browser : Browser) = getState
//      return browser.Button(Find.ByName(buttonText)).Click() }
//
//    let closePage = state {
//      let! (browser : Browser) = getState
//      return browser.Dispose() }
//
//    //usage 3 -  special usage even for run
//    let evalState m s = runState m s |> fst
//    let runScript (script:State<'a,Browser>) =
//      evalState script (new IE() :> Browser)
//
//
//    //usage 3 -  compose into bigger expression and run
//    [<Fact>]
//    let ``Can find CodeBetter on Bing``() =
//      runScript ( 
//        state {
//          do! openPage "http://bing.com"
//          do! enterText "q" "CodeBetter"
//          do! clickButton "go"
//          let! result = containsText "CodeBetter"
//          isTrue result
//          do! closePage })

//
//Reader monad - 5'
module Reader = 

    //READER monad
    //but we never modify the state monad above, we just read it.
    //why give it back in the type ? this is the reader monad


    //1- write the type
    type Reader<'env,'a> = Reader of ('env -> 'a)

    //2- write how to run the monad
    let runReader (Reader r) env  = r env
    let runReader2 env (Reader r) = r env

    //3- write composition rules
    type ReaderBuilder() =
      member this.Return     a  = Reader (fun _ -> a)
      member this.Bind(m, k) = 
        (*construct a new monad object *) 
        Reader (fun r ->              (*here my goal is to get back one 'b*) 
                   runReader2 r m     (*1- I execute the first monad M<'a> to get back 'a *) 
                   |> k               (*2- I pass on the result to k to get back a new monad M<'b>*)
                   |> runReader2 r )  (*3- which I execute to get a 'b*)
      member this.ReturnFrom (a:Reader<'env,'a>)  = a
      member this.Delay (f:unit -> Reader<'r,'a>) =  this.Bind(this.Return (), f)//Reader (fun () -> runReader (f())) 
      member this.Zero() = this.Return ()
      member this.While (guard:unit -> bool,  m: Reader<'r,unit>) : Reader<'r,unit> =   
           if not (guard()) then this.Zero() else this.Bind( m, fun () -> this.While(guard, m)) 
      member this.Combine (m1,m2) = this.Bind(m1, fun () -> m2) //: M<'a> * M<'a> -> M<'a> 

    let reader = new ReaderBuilder()                                         


    //the reader capture the essential part of what "reading something" means, and allow compositionality of such notion.
    //but it is up to each appplication to choose which concrete meaning it will give, thanks to primitives.



    //usage 1 : LOCKING  : we want to read from an object, but insert some control logic to this read.
    //this is a task for the reader monad
    open System.Threading


    //usage 1  -  specialise usage into monadic primitive 
    //we try to run a reader, with some locking logic
    let tryRunLock lock m =
      let lock = box lock
      let lockTaken = ref false
      Monitor.Enter(lock, lockTaken) 

      match !lockTaken with
      | true -> try Some(runReader m lock)  //we try to read
                finally Monitor.Exit lock   //and we release the lock
      | _ -> None

    let pulseAll = Reader Monitor.PulseAll
    let wait = Reader (fun env -> Monitor.Wait env |> ignore)


    open System.Collections.Generic

    let pop (stack:Stack<_>) = reader {
                                    while stack.Count = 0 do 
                                     return! wait
                                    return stack.Pop() }

    let push (stack:Stack<_>) x = reader {
      if stack.Count = 0 then return! pulseAll
      do stack.Push(x) }

    let lockObj = new obj()

    let move s1 s2 = 
      reader { 
        let! x = pop s1 
        do! push s2 x
        return x } 
      |> tryRunLock lockObj


    let s1 = new Stack<int>([1..3])
    let s2 = new Stack<int>()
    let moved = move s1 s2
    s2 |> ignore
    s1 |> ignore


// 
//    //usage 2  -  specialise usage into monadic primitive
//    let ask     = Reader (id)                                //read the environnement from the environment
//    let asks  f = reader { let! r = ask in return (f r) }    //read (f environment) from the environement
//    let local f m = Reader (f >> runReader m)                //read m from (f environment)
//
//
//
//    let openPage (url:string) = reader { let! (browser : Browser) = ask
//                                         return browser.GoTo url }
//
//    let openPage (url:string) = Reader ( fun (browser : Browser) -> browser.GoTo url)
//    let openPage (url:string) = reader { return! asks (fun (browser : Browser) -> browser.GoTo url) }
//
//
//    //usage 2 -  compose into bigger expression and run
//    [<Fact>]
//    let ``Can find CodeBetter on Bing``() =
//      reader {
//        do! openPage "http://bing.com"
//        do! enterText "q" "CodeBetter"
//        do! clickButton "go"
//        let! result = containsText "CodeBetter"
//        isTrue result
//        do! closePage } |> runScript
//



