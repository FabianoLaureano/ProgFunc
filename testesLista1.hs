import Test.HUnit

-- comando para rodar os testes -> runTestTT ListaDeTestes

t1Xor = TestCase (assertEqual "t" True (xor True False))
t2Xor = TestCase (assertEqual "t" True (xor False True))
t3Xor = TestCase (assertEqual "t" False (xor True True))
t4Xor = TestCase (assertEqual "t" False (xor False False))

testesXor = TestList [t1Xor, t2Xor, t3Xor, t4Xor]

t1Impl = TestCase (assertEqual "t" False (impl True False))
t2Impl = TestCase (assertEqual "t" True (impl False True))
t3Impl = TestCase (assertEqual "t" True (impl True True))
t4Impl = TestCase (assertEqual "t" True (impl False False))

testesImpl = TestList [t1Impl, t2Impl, t3Impl, t4Impl]

t1Equiv = TestCase (assertEqual "t" False (equiv True False))
t2Equiv = TestCase (assertEqual "t" False (equiv False True))
t3Equiv = TestCase (assertEqual "t" True (equiv True True))
t4Equiv = TestCase (assertEqual "t" True (equiv False False))

testesImpl = TestList [t1Equiv, t2Equiv, t3Equiv, t4Equiv]