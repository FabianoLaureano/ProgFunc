import Test.HUnit
import System.Environment

{-
- Usando os predicados not,and e or prontos de Haskell, implemente os predicados (funcoes) xor (or exclusivo),
- impl (implicacao A => B é equivalente a (not A or B)) e equiv (A <=> B é definido como A => B and B => A)
- Procure usar casamento de padroes e reutilizar as funcoes.
-}
xor :: Bool -> Bool -> Bool
-- xor a b = (a || b) && not (a && b)
xor True a = not a
xor False a = a

----------------------------------------

impl :: Bool -> Bool -> Bool
impl a b = (not a) || b

----------------------------------------

{-
A funcao square esta implementada e eleva ao quadrado um determinado numero
-}
square x = x*x

equiv :: Bool -> Bool -> Bool
equiv a b = (impl a b) && (impl b a)

----------------------------------------

{-
- Implemente a funcao potencia, que retorna o resultado de x elevado a y 
-}
pow x 0 = 1
pow x y
  | y > 0 = x*(pow x (y-1))
  | otherwise = 1/(pow x (-y))


----------------------------------------

{-
- Implemente a funcao fatorial que calcula o fatorial de um numero 
-}

fatorial :: Int -> Int
fatorial 0 = 1
fatorial a = a * fatorial (a-1)

----------------------------------------

{-
- Determina se um numero eh primo ou nao. Preocupe-se apenas em resolver o problema.
- Nao precisa usar conhecimentos mais sofisticados da teoria dos numeros. Voce pode trabalhar com listas.
-}

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime n | (length [x | x <- [2 .. n-1], mod n x == 0]) > 0 = False
  | otherwise = True

----------------------------------------

{-
- Calcula um termo da sequencia de Fibonnacci. Voce pode trabalhar com listas. 
-}

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib a = fib (a-1) + fib (a-2)

----------------------------------------

{-
- Calcula um MDC de dois numeros usando o algoritmo de Euclides. 
-}

mdc :: Int -> Int -> Int
mdc a 0 = a
mdc a b = mdc b (mod a b)

----------------------------------------

{-
- Calcula um MMC de dois numeros. 
-}

mmc :: Int -> Int -> Int
mmc a 0 = 0
mmc 0 b = 0
mmc a b = div (a * b) (mdc a b)

----------------------------------------

{-
- Determina se dois numeros inteiros positivos sao co-primos. Dois numeros sao co-primos se 
- o mdc deles for igual a 1. Ex: coprimo 35 64 = True 
-}

coprimo :: Int -> Int -> Bool
coprimo a b | (mdc a b /= 1) = False
  | otherwise = True

{-
- Calcula a conjectura de Goldbach, que diz que um numero par maior que 2 pode ser escrito como a soma de dois numeros primos. Ex: 28 = 5 + 23.
-}
goldbach x = [ (y,z)| y <- filter isPrime [1..(x-1)], z <- filter isPrime [1..(x-1)], y + z == x ]

--------------------------------------------------------------------------------------------------------------------------------

-- comando para rodar os testes -> runTestTT ListaDeTestes

data TData = TString String
		     | TNumber Double
		     | TBool Bool
		     | TNull
             deriving (Show, Eq, Ord)

-- testar se os tipos dos parametros do xor são Booleanos
tTipoP :: (Bool -> Bool -> Bool) -> TData -> TData -> Maybe Bool
tTipoP f (TBool a) (TBool b) = Just (f a b)
tTipoP f _ _ = Nothing

a = TBool True 
b = TNumber 5
c = TBool False
d = TNull

t1Xor = TestCase (assertEqual "t1Xor" True (xor True False))
t2Xor = TestCase (assertEqual "t2Xor" True (xor False True))
t3Xor = TestCase (assertEqual "t3Xor" False (xor True True))
t4Xor = TestCase (assertEqual "t4Xor" False (xor False False))

t5Xor = TestCase (assertEqual "teste do xor com Bool e Number" Nothing (tTipoP xor a b))
t6Xor = TestCase (assertEqual "teste do xor com null e Number" Nothing (tTipoP xor d b))
t7Xor = TestCase (assertEqual "teste do xor com Bool" (Just True) (tTipoP xor a c))

testesXor = TestList [t1Xor, t2Xor, t3Xor, t4Xor, t5Xor, t6Xor, t7Xor]

---------------------------------------------------------------------

t1Impl = TestCase (assertEqual "t1Impl" False (impl True False))
t2Impl = TestCase (assertEqual "t2Impl" True (impl False True))
t3Impl = TestCase (assertEqual "t3Impl" True (impl True True))
t4Impl = TestCase (assertEqual "t4Impl" True (impl False False))

testesImpl = TestList [t1Impl, t2Impl, t3Impl, t4Impl]

t1Equiv = TestCase (assertEqual "t1Equiv" False (equiv True False))
t2Equiv = TestCase (assertEqual "t2Equiv" False (equiv False True))
t3Equiv = TestCase (assertEqual "t3Equiv" True (equiv True True))
t4Equiv = TestCase (assertEqual "t4Equiv" True (equiv False False))

testesEquiv = TestList [t1Equiv, t2Equiv, t3Equiv, t4Equiv]

---------------------------------------------------------------------

t1IsPrime = TestCase (assertEqual "t1IsPrime" False (isPrime 0))
t2IsPrime = TestCase (assertEqual "t2IsPrime" False (isPrime 1))
t3IsPrime = TestCase (assertEqual "t3IsPrime" False (isPrime 4))
t4IsPrime = TestCase (assertEqual "t4IsPrime" False (isPrime 6))
t5IsPrime = TestCase (assertEqual "t5IsPrime" False (isPrime 8))
t6IsPrime = TestCase (assertEqual "t6IsPrime" False (isPrime 10))
t7IsPrime = TestCase (assertEqual "t7IsPrime" False (isPrime 500))
t8IsPrime = TestCase (assertEqual "t8IsPrime" False (isPrime 1000))

t9IsPrime = TestCase (assertEqual "t9IsPrime" True (isPrime 2))
t10IsPrime = TestCase (assertEqual "t10IsPrime" True (isPrime 3))
t11IsPrime = TestCase (assertEqual "t11IsPrime" True (isPrime 5))
t12IsPrime = TestCase (assertEqual "t12IsPrime" True (isPrime 7))
t13IsPrime = TestCase (assertEqual "t13IsPrime" True (isPrime 11))
t14IsPrime = TestCase (assertEqual "t14IsPrime" True (isPrime 13))
t15IsPrime = TestCase (assertEqual "t15IsPrime" True (isPrime 17))
t16IsPrime = TestCase (assertEqual "t16IsPrime" True (isPrime 19))
t17IsPrime = TestCase (assertEqual "t17IsPrime" True (isPrime 991))
t18IsPrime = TestCase (assertEqual "t18IsPrime" True (isPrime 392113))

testesIsPrime = TestList [t1IsPrime, t2IsPrime, t3IsPrime, t4IsPrime, t5IsPrime, t6IsPrime, t7IsPrime, t8IsPrime, t9IsPrime, t10IsPrime, t11IsPrime, t12IsPrime, t13IsPrime, t14IsPrime, t15IsPrime, t16IsPrime, t17IsPrime, t18IsPrime]

---------------------------------------------------------------------

t1Mdc = TestCase (assertEqual "t1Mdc" 1 (mdc 1 0))
t2Mdc = TestCase (assertEqual "t2Mdc" 8 (mdc 24 16))
t3Mdc = TestCase (assertEqual "t3Mdc" 1 (mdc 25 26))
t4Mdc = TestCase (assertEqual "t4Mdc" 9 (mdc 27 9))
t5Mdc = TestCase (assertEqual "t5Mdc" 12 (mdc 12 36))

testesMdc = TestList [t1Mdc, t2Mdc, t3Mdc, t4Mdc, t5Mdc]

---------------------------------------------------------------------

t1Mmc = TestCase (assertEqual "t1Mmc" 0 (mmc 1 0))
t2Mmc = TestCase (assertEqual "t2Mmc" 0 (mmc 0 20))
t3Mmc = TestCase (assertEqual "t3Mmc" 12 (mmc 4 6))
t4Mmc = TestCase (assertEqual "t4Mmc" 36 (mmc 12 9))
t5Mmc = TestCase (assertEqual "t5Mmc" 2 (mmc 2 2))

testesMmc = TestList [t1Mmc, t2Mmc, t3Mmc, t4Mmc, t5Mmc]

---------------------------------------------------------------------

t1Square = TestCase (assertEqual "t1Square" 100 (square (-10)))
t2Square = TestCase (assertEqual "t2Square" 25 (square (-5)))
t3Square = TestCase (assertEqual "t3Square" 1 (square (-1)))
t4Square = TestCase (assertEqual "t4Square" 0 (square (0)))
t5Square = TestCase (assertEqual "t5Square" 1 (square (1)))
t6Square = TestCase (assertEqual "t6Square" 25 (square (5)))
t7Square = TestCase (assertEqual "t7Square" 100 (square (10)))
t8Square = TestCase (assertEqual "t8Square" 1522756 (square (1234)))

testesSquare = TestList [t1Square, t2Square, t3Square, t4Square, t5Square, t6Square, t7Square, t8Square]

---------------------------------------------------------------------

t1Pow = TestCase (assertEqual "t1Pow" 1 (pow 2 0))
t2Pow = TestCase (assertEqual "t2Pow" 2 (pow 2 1))
t3Pow = TestCase (assertEqual "t3Pow" (-8) (pow (-2) 3))
t4Pow = TestCase (assertEqual "t4Pow" 0.2 (pow 5 (-1)))
t5Pow = TestCase (assertEqual "t5Pow" 1 (pow 1 (-1)))
t6Pow = TestCase (assertEqual "t6Pow" (-1) (pow (-1) (-1)))
t7Pow = TestCase (assertEqual "t7Pow" 1e-10 (pow (-10) (-10)))

testesPow = TestList [t1Pow, t2Pow, t3Pow, t4Pow, t5Pow, t6Pow, t7Pow]

---------------------------------------------------------------------

t1Fatorial = TestCase (assertEqual "t1Fatorial" 1 (fatorial 0))
t2Fatorial = TestCase (assertEqual "t2Fatorial" 120 (fatorial 5))
t3Fatorial = TestCase (assertEqual "t3Fatorial" 5040 (fatorial 7))
t4Fatorial = TestCase (assertEqual "t4Fatorial" 362880 (fatorial 9))
t5Fatorial = TestCase (assertEqual "t5Fatorial" 3628800 (fatorial 10))
t6Fatorial = TestCase (assertEqual "t6Fatorial" 2432902008176640000 (fatorial 20))

testesFatorial = TestList [t1Fatorial, t2Fatorial, t3Fatorial, t4Fatorial, t5Fatorial, t6Fatorial]

---------------------------------------------------------------------

t1Fib = TestCase (assertEqual "t1Fib" 1 (fib 1))
t2Fib = TestCase (assertEqual "t2Fib" 2 (fib 3))
t3Fib = TestCase (assertEqual "t3Fib" 5 (fib 5))
t4Fib = TestCase (assertEqual "t4Fib" 55 (fib 10))
t5Fib = TestCase (assertEqual "t5Fib" 987 (fib 16))
t6Fib = TestCase (assertEqual "t6Fib" 10946 (fib 21))
t7Fib = TestCase (assertEqual "t7Fib" 121393 (fib 26))

testesFib = TestList [t1Fib, t2Fib, t3Fib, t4Fib, t5Fib, t6Fib, t7Fib]

---------------------------------------------------------------------

t1Coprimo = TestCase (assertEqual "t1Coprimo" True (coprimo 5 7))
t2Coprimo = TestCase (assertEqual "t2Coprimo" True (coprimo 11 49))
t3Coprimo = TestCase (assertEqual "t3Coprimo" True (coprimo 36 91))
t4Coprimo = TestCase (assertEqual "t4Coprimo" False (coprimo 234 658))
t5Coprimo = TestCase (assertEqual "t5Coprimo" True (coprimo 20 21))
t6Coprimo = TestCase (assertEqual "t6Coprimo" False (coprimo 0 5))
t7Coprimo = TestCase (assertEqual "t7Coprimo" False (coprimo 15 63))
t8Coprimo = TestCase (assertEqual "t8Coprimo" False (coprimo 66 8))

testesCoprimo = TestList [t1Coprimo, t2Coprimo, t3Coprimo, t4Coprimo, t5Coprimo, t6Coprimo, t7Coprimo, t8Coprimo]

---------------------------------------------------------------------

t1Goldbach = TestCase (assertEqual "t1Goldbach" [] (goldbach 3))
t2Goldbach = TestCase (assertEqual "t2Goldbach" [(5,73),(7,71),(11,67),(17,61),(19,59),(31,47),(37,41),(41,37),(47,31),(59,19),(61,17),(67,11),(71,7),(73,5)] (goldbach 78))
t3Goldbach = TestCase (assertEqual "t3Goldbach" [(3,5),(5,3)] (goldbach 8))
t4Goldbach = TestCase (assertEqual "t4Goldbach" [(3,61),(5,59),(11,53),(17,47),(23,41),(41,23),(47,17),(53,11),(59,5),(61,3)] (goldbach 64))
t5Goldbach = TestCase (assertEqual "t5Goldbach" [(3,29),(13,19),(19,13),(29,3)] (goldbach 32))
t6Goldbach = TestCase (assertEqual "t6Goldbach" [] (goldbach 97))
t7Goldbach = TestCase (assertEqual "t7Goldbach" [(3,61),(5,59),(11,53),(17,47),(23,41),(41,23),(47,17),(53,11),(59,5),(61,3)] (goldbach 64))

testesGoldbach = TestList [t1Goldbach, t2Goldbach, t3Goldbach, t4Goldbach, t5Goldbach, t6Goldbach, t7Goldbach]

---------------------------------------------------------------------

testesGerais = TestList [testesXor, testesImpl, testesEquiv, testesIsPrime, testesMdc, testesMmc, testesSquare, testesPow, testesFatorial, testesFib, testesCoprimo, testesGoldbach]

main = do
  tests <- (runTestTT testesGerais)
  let resultados = words (showCounts tests)
  let matricula = "XXXXXXXX"
  let totalDeTestes = resultados !! 1
  let testesComErros = resultados !! 5
  let testesComFalhas = resultados !! 7
  let testesQuePassaram = ((read (resultados !! 1)) - (read (resultados !! 5)) -  (read (resultados !! 7)))
  let jsonFile = ("{matricula:" ++ matricula ++ ",totalTestes:" ++ totalDeTestes++ ",erros:" ++ testesComErros ++ ",falhas:" ++ testesComFalhas ++ ",passaram:" ++ (show testesQuePassaram) ++ "}")
  writeFile "result.json" jsonFile