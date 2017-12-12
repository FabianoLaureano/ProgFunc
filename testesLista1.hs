import Test.HUnit

-- comando para rodar os testes -> runTestTT ListaDeTestes

t1Xor = TestCase (assertEqual "t" True (xor True False))
t2Xor = TestCase (assertEqual "t" True (xor False True))
t3Xor = TestCase (assertEqual "t" False (xor True True))
t4Xor = TestCase (assertEqual "t" False (xor False False))

testesXor = TestList [t1Xor, t2Xor, t3Xor, t4Xor]

---------------------------------------------------------------------

t1Impl = TestCase (assertEqual "t" False (impl True False))
t2Impl = TestCase (assertEqual "t" True (impl False True))
t3Impl = TestCase (assertEqual "t" True (impl True True))
t4Impl = TestCase (assertEqual "t" True (impl False False))

testesImpl = TestList [t1Impl, t2Impl, t3Impl, t4Impl]

t1Equiv = TestCase (assertEqual "t" False (equiv True False))
t2Equiv = TestCase (assertEqual "t" False (equiv False True))
t3Equiv = TestCase (assertEqual "t" True (equiv True True))
t4Equiv = TestCase (assertEqual "t" True (equiv False False))

testesEquiv = TestList [t1Equiv, t2Equiv, t3Equiv, t4Equiv]

---------------------------------------------------------------------

t1IsPrime = TestCase (assertEqual "t" False (isPrime 0))
t2IsPrime = TestCase (assertEqual "t" False (isPrime 1))
t3IsPrime = TestCase (assertEqual "t" False (isPrime 4))
t4IsPrime = TestCase (assertEqual "t" False (isPrime 1000))

t5IsPrime = TestCase (assertEqual "t" True (isPrime 2))
t6IsPrime = TestCase (assertEqual "t" True (isPrime 7))
t7IsPrime = TestCase (assertEqual "t" True (isPrime 991))
t8IsPrime = TestCase (assertEqual "t" True (isPrime 392113))

testesIsPrime = TestList [t1IsPrime, t2IsPrime, t3IsPrime, t4IsPrime, t5IsPrime, t6IsPrime, t7IsPrime, t8IsPrime]

---------------------------------------------------------------------

t1Mdc = TestCase (assertEqual "t" 1 (mdc 1 0))
t2Mdc = TestCase (assertEqual "t" 8 (mdc 24 16))
t3Mdc = TestCase (assertEqual "t" 1 (mdc 25 26))
t4Mdc = TestCase (assertEqual "t" 9 (mdc 27 9))

testesMdc = TestList [t1Mdc, t2Mdc, t3Mdc, t4Mdc]

---------------------------------------------------------------------

t1Mmc = TestCase (assertEqual "t1" 0 (mmc 1 0))
t2Mmc = TestCase (assertEqual "t2" 0 (mmc 0 20))
t3Mmc = TestCase (assertEqual "t3" 12 (mmc 4 6))

testesMmc = TestList [t1Mmc, t2Mmc, t3Mmc]

---------------------------------------------------------------------

t1Square = TestCase (assertEqual "t" 100 (square (-10)))
t2Square = TestCase (assertEqual "t" 25 (square (-5)))
t3Square = TestCase (assertEqual "t" 1 (square (-1)))
t4Square = TestCase (assertEqual "t" 0 (square (0)))
t5Square = TestCase (assertEqual "t" 1 (square (1)))
t6Square = TestCase (assertEqual "t" 25 (square (5)))
t7Square = TestCase (assertEqual "t" 100 (square (10)))

testesSquare = TestList [t1Square, t2Square, t3Square, t4Square, t5Square, t6Square, t7Square]

---------------------------------------------------------------------

t1Pow = TestCase (assertEqual "t" 1 (pow 2 0))
t2Pow = TestCase (assertEqual "t" 2 (pow 2 1))
t3Pow = TestCase (assertEqual "t" (-8) (pow (-2) 3))
t4Pow = TestCase (assertEqual "t" 0.2 (pow 5 (-1)))
t5Pow = TestCase (assertEqual "t" 1 (pow 1 (-1)))
t6Pow = TestCase (assertEqual "t" (-1) (pow (-1) (-1)))
t7Pow = TestCase (assertEqual "t" 1e-10 (pow (-10) (-10)))

testesPow = TestList [t1Pow, t2Pow, t3Pow, t4Pow, t5Pow, t6Pow, t7Pow]

---------------------------------------------------------------------

t1Fatorial = TestCase (assertEqual "t" 1 (fatorial 0))
t2Fatorial = TestCase (assertEqual "t" 120 (fatorial 5))
t3Fatorial = TestCase (assertEqual "t" 5040 (fatorial 7))
t4Fatorial = TestCase (assertEqual "t" 362880 (fatorial 9))
t5Fatorial = TestCase (assertEqual "t" 3628800 (fatorial 10))
t6Fatorial = TestCase (assertEqual "t" 2432902008176640000 (fatorial 20))

testesFatorial = TestList [t1Fatorial, t2Fatorial, t3Fatorial, t4Fatorial, t5Fatorial, t6Fatorial]

---------------------------------------------------------------------
