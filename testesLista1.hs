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