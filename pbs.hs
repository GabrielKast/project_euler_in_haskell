import Data.List (union)
import Data.Char (ord)


pb1 = sum [x | x<-[1..999], x `mod` 3==0 || x `mod` 5 ==0]

         
pb2 limit = pb2_list 1 limit

pb2_list n limit
  | fib > limit = 0
  | fib `mod`2== 0 = fib + pb2_list (n+1) limit
  | otherwise = pb2_list (n+1) limit
    where fib = fibo n
          
pb2' limit = sum(pb2_list' 1 limit)
  
pb2_list' n limit
  | fib > limit = []
  | fib `mod` 2==0 = fib : pb2_list' (n+1) limit
  | otherwise = pb2_list' (n+1) limit
    where fib = fibo n
             
                
pb2'' limit = sum(filter (even) (takeWhile(<limit) $ map fibo [1..]))
                
pb3 n = head (reverse(dividors n)) 

pb4 = maximum [x*y | x <- [100..999], y <- [100..999], x<=y, palindrom (x*y)]

pb5 = pb5' 2
pb5' n = if (divisible20 n) then n else (pb5' (n+2))

divisible20 n = all (\m -> n `mod` m ==0 ) [1..20]

pb5'' = foldl (*) 1 (unions [dividors n | n <- [1..20]])


pb6 = pb6' 100 
pb6' x =  (sum [x | x <- [1..x]]) ^ 2 - sum [x*x | x <- [1..x]]

pb7 = pb7' 10001
pb7' n = primes !! (n-1)

pb8 = maximum [(product_to_5 i s) | i <- [0.. (length s)-5] ]
  where s="7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"

product_to_5::Int -> String -> Int
product_to_5 index s = foldl (\acc i -> acc * (ord(s !! (index+i)) - ord '0')) 1 [0..4]

pb9 = head [ a * b * c |  a<- [1..998], b<-[1..998], c<-[1..998],
             a<b, b<c, a + b + c == 1000, a^2+b^2==c^2]

pb10 = sum(takeWhile (<2000000) primes)

fibo 1 = 1
fibo 2 = 2
fibo n = fibo (n-2) + fibo(n-1) 

dividors n = dividors' n primes

dividors' :: Integer -> [Integer] -> [Integer]
dividors' n (p:ps)
  | n == 1 = []
  | n `mod` p == 0 = p : dividors' (n `div` p) (p:ps)
  | otherwise = dividors' n ps

all_dividors n = filter (\x -> n `mod` x ==0) [1..n]


-- dividors_mine n = filter (\x -> is_prime' x && n `mod` x==0) (takeWhile (<=round (sqrt (fromIntegral n))) (2:[3, 5..]))
-- is_prime' :: Integer -> Bool             
-- is_prime' n = all (\x -> n `mod` x /=0) (takeWhile (<=round (sqrt (fromIntegral n))) (2:[3, 5..]))

sieve (x:xs) = x : (sieve (filter (\y -> y `mod` x /= 0) xs))
primes = sieve (2:[3, 5..])

palindrom n = (read (reverse (show n))) == n

-- union xs [] = xs
-- union [] ys = ys
-- union (x:xs) (y:ys)
--   | x == y = x : union xs ys
--   | x < y = x: union xs (y:ys)
--   | otherwise = y: union (x:xs) ys

unions [] = []
unions xxs = union (head xxs) (unions (tail xxs))
