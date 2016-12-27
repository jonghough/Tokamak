namespace TokamakTests
open System
open NUnit.Framework
open Tokamak.Core


[<TestFixture>]
type TokamakTest() = 

    [<Test>]
    member x.TestCase1() =
        let script1 = """
            function fib(x)
                if x <= 1 then
                    1
                else 
                    x * fib(x-1)
                end
            end

            f = fib(5)
        """
        let comp = new Compiler()
        let exp = comp.compile(script1)
        let myReactor = new IntegerConfinementUnit(exp)

        Assert.IsTrue(myReactor.R = 120L)

    [<Test>]
    member x.TestCase2() =
        let script1 = """
            function fib(x)
                if x <= 1 then
                    1
                else 
                    x * fib(x-1)
                end
            end

            a = 1$4
            for d in 0$3 do
                a[d] = fib(d)
            end
            a
        """
        let comp = new Compiler()
        let exp = comp.compile(script1)
        let myReactor = new ArrayConfinementUnit(exp)
        let l = myReactor.R
        let m : int64 list = List.map (fun (a : Expression) -> (new IntegerConfinementUnit(a)).R) l

        Assert.IsTrue(m.[0] = 1L)
        Assert.IsTrue(m.[1] = 1L)
        Assert.IsTrue(m.[2] = 2L)
        Assert.IsTrue(m.[3] = 6L)

    
    [<Test>]
    member x.TestCase3() =
        let script1 = """
            a = 3$100
            b = 3 + a[0]
            b
        """
        let comp = new Compiler()
        let exp = comp.compile(script1)
        let myReactor = new IntegerConfinementUnit(exp)

        Assert.IsTrue(myReactor.R = 6L)


    [<Test>]
    member x.TestCase4() =
        let script1 = """
            a = 90$100
            a[8] = 5
           
            x = 103
        """
        let comp = new Compiler()
        let exp = comp.compile(script1)
        let myReactor = new IntegerConfinementUnit(exp)

        Assert.IsTrue(myReactor.R = 103L)

    [<Test>]
    member x.TestCase5() =
        let script1 = """
            a = 5$10
            k = a[0] + a[1] + a[2] + a[3]
           
        """
        let comp = new Compiler()
        let exp = comp.compile(script1)
        let myReactor = new IntegerConfinementUnit(exp)

        Assert.IsTrue(myReactor.R = 26L)

    
    [<Test>]
    member x.TestCase6() =
        let script1 = """
            function myFunc(a, b, c) a + b + c end

            myFunc("hello",", ", "world!")
           
        """
        let comp = new Compiler()
        let exp = comp.compile(script1)
        let myReactor = new LiteralConfinementUnit(exp)
        Assert.IsTrue(myReactor.R = "hello, world!")

    [<Test>]
    member x.TestCase7() =
        let script1 = """
            function doit(a) a + 4.01 end

            array = 0$100
            doit(array[4])
           
        """
        let comp = new Compiler()
        let exp = comp.compile(script1)
        let myReactor = new FloatConfinementUnit(exp)

        Assert.IsTrue(myReactor.R = 8.01)

    [<Test>]
    member x.TestCase8() =
        let script1 = """
            function doit(a) 
                 300.5  / doother(a)
            end

            function doother(a) a ^ 2.4 end

            doit(4.5)
           
        """
        let comp = new Compiler()
        let exp = comp.compile(script1)

       
        let myReactor = new FloatConfinementUnit(exp)
        Assert.IsTrue(Math.Abs(myReactor.R - 8.13082180691237) < 0.0001)


    [<Test>]
    member x.TestCase9() =
        let script1 = """
            function doit() 
                array = 1$100
                a = 0
                for i in array do
                    a = a - array[i-1]
                end
                a
            end

            doit()
           
        """
        let comp = new Compiler()
        let exp = comp.compile(script1)

       
        let myReactor = new IntegerConfinementUnit(exp)
        Assert.IsTrue(myReactor.R = -5050L)

    
    [<Test>]
    member x.TestCase10() =
        let script1 = """
            function whilefunction(a, b) 
                if a <= 0 then
                    a = -a
                end
                returnValue = 0
                while a > 0 do
                    a = a - 1
                    returnValue = returnValue + b
                end
            end

            whilefunction(10, 100)
           
        """
        let comp = new Compiler()
        let exp = comp.compile(script1)

       
        let myReactor = new IntegerConfinementUnit(exp)
        Assert.IsTrue(myReactor.R = 1000L)

    [<Test>]
    member x.TestCase11() =
        let script1 = """
            function square(x) 
                x * x
            end

            m = 0$2

            m[0] = square(10)
            m[0]
           
        """
        let comp = new Compiler()
        let exp = comp.compile(script1)

       
        let myReactor = new IntegerConfinementUnit(exp)
        Assert.IsTrue(myReactor.R = 100L)

    [<Test>]
    member x.TestCase12() =
        let script1 = """
            function square(x) 
                x * x
            end

            m = 0$2

            m[0] = square
            m[0](10)
           
        """
        let comp = new Compiler()
        let exp = comp.compile(script1)

       
        let myReactor = new IntegerConfinementUnit(exp)
        Assert.IsTrue(myReactor.R = 100L)

    
    [<Test>]
    member x.TestCase13() =
        let script1 = """
            function iseven(n) (n%2) == 0 end

arr = 0$100

for i in 0$100 do
  arr[i] = iseven(i)
end

arr
           
        """
        let comp = new Compiler()
        let exp = comp.compile(script1)

       
        let myReactor = new ArrayConfinementUnit(exp)
        let mutable ctr = 0
        List.iter (fun e -> 
                        let r = comp.EvaluateExpression e
                        let bcu = new BoolConfinementUnit(r)
                        if 0 =  ctr % 2 then
                            Assert.IsTrue(bcu.R = true)
                        else 
                            Assert.IsTrue(bcu.R = false)
                        ctr <- ctr + 1) myReactor.R

        