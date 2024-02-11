-- 定义函数

-- 函数签名 用来标注函数的类型
f1 :: Int -> Int -> Int
-- 函数实现
f1 x y = x + y

-- let 表达式

f2 =
  let x = 1
      y = 2
      z = 3
   in x + y + z
-- >>> f2
-- 6

f3 x = if x == 0 then False else True
-- >>>f3 0
-- False

-- case 表达式
f4 x = case x of
  0 -> False
  _ -> True
-- guard 守卫
f5 x = case x of
  _
    | x /= 1 -> False
    | otherwise -> True

-- do 表达式
f6 = Just 1 >>= \x -> Just x

-- >>> :t Just 1
-- Just 1 :: Num a => Maybe a
f7 :: Maybe Integer
f7 = do
  x <- Just 1
  return x

-- data 关键字 用来创建ADT

data K = K1 Int Int | K2  Int deriving (Eq,Show)

-- >>>  K2 1 
-- K2 1

main :: IO ()
main = do 
  putStrLn "hello ,world!"