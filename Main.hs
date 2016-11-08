
module Main where
import Control.Monad.Free
import Control.Comonad
import Control.Comonad.Cofree
-- f :: * -> *
-- Dumb a :: * -> *
data Dumb b next = Output b next
                 | Ring next
                 | Done
  deriving Show
instance Functor (Dumb a) where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f (Output val next) = Output val (f next)
  fmap f (Ring next) = Ring (f next)
  fmap _ (Done) = Done

type DumbProgram b = Free (Dumb b) ()
-- data Free f r = Free (f (Free f r))
--               | Pure r
--dumbVal = Free ((Output 'a') (Free (Ring (Free Done))
output x = Free $ Output x  (Pure ())
ring = Free $ Ring (Pure ())
done = Free $ Done

dumbProgram :: DumbProgram String
dumbProgram = do
  output "a"
  ring

-- instance Functor f => Monad (Free f) where
--   return = pure
--   {-# INLINE return #-}
--   Pure a >>= f = f a
--   Free m >>= f = Free (fmap (>>= f) m)
--   Free m >>= f = Free (m >>= \x -> return (x >>= f))

dumbProgram'  = (Free (Output "a"  (Pure ()))) >>= \_ -> (Free ( Ring (Pure ())))


dumbInterpreter :: (Show a) => DumbProgram a -> IO ()
dumbInterpreter (Free (Output x next)) = do
  putStr $ show x
  dumbInterpreter next
dumbInterpreter (Free (Ring next)) = do
  putStr "\a"
  dumbInterpreter next
dumbInterpreter (Free Done) = return ()

dumbInterpreter (Pure ()) = return ()


data Calc next = Add Int next
               | Times Int next
               | Sub Int next
               | Clear next
               | Choice next next
               | Subtotal (Int -> next)


instance Functor (Calc) where
  fmap f (Add a next) = Add a (f next)
  fmap f (Sub a next) = Sub a (f next)
  fmap f (Times a next) = Times a (f next)
  fmap f (Clear next) = Clear (f next)
  fmap f (Choice c c') = Choice (f c) (f c)
  fmap f (Subtotal g) = Subtotal (f . g)

type CalcProg = Free Calc

add :: Int -> CalcProg Int
add a = Free $ Add a (Pure a) -- could be a fn?
sub :: Int -> CalcProg Int
sub a = Free $ Sub a (Pure a)
times :: Int -> CalcProg Int
times a = Free $ Times a (Pure a)
clear :: CalcProg Int
clear = Free $ Clear (Pure 0)

choice :: CalcProg a -> CalcProg a -> CalcProg a
choice c c' = Free $ Choice c c'

subtotal :: CalcProg Int
subtotal = Free $ (fmap Pure (Subtotal id))

calculate :: CalcProg Int -> Int
calculate (Free (Add a next)) = a + (calculate next)
calculate (Free (Sub a next)) = (calculate next) - a
calculate (Free (Times a next)) = a * (calculate next)
calculate (Free (Clear next)) = (calculate next)
calculate (Pure n) = n

calculate2 :: Free Calc Int -> Int -> Int
calculate2 (Free (Add a next)) i = calculate2 next (a + i)
calculate2 (Free (Sub a next)) i = calculate2 next (i - a)
calculate2 (Free (Times a next)) i = calculate2 next (i * a)
calculate2 (Free (Clear next)) _ = calculate2 next 0
calculate2 (Free (Choice next next')) i = calculate2 next i
calculate2 (Free (Subtotal g)) i = calculate2 (g i) i
calculate2 (Pure n) i = i



cprog :: CalcProg Int
cprog = do
  add 5
  v <- subtotal
  times (v+1)


-- comonad example
type Flags = String
data Config = MkConfig [Flags] deriving (Show,Eq)

configBuilder :: [Flags] -> Config
configBuilder = MkConfig

defaultConfig :: [Flags] -> Config
defaultConfig flgs = MkConfig ("-v" : flgs)

profile :: ([Flags] -> Config) -> Config
profile builder = builder ["-a", "-debug"]

optimize :: ([Flags] -> Config) -> Config
optimize builder = builder ["-O2"]

args :: [Flags] -> ([Flags] -> Config) -> Config
args flags builder = builder flags
-- profile uses up the builder and then we can add to the config anymore
--   without pattern matching on the data constructor!

profile' :: ([Flags] -> Config) -> ([Flags] -> Config)
profile' builder = \flgs -> builder $ ["-a", "-debug"] ++ flgs

optimize' :: ([Flags] -> Config) -> ([Flags] -> Config)
optimize' builder = \flgs -> builder $ "-O2" : flgs

myExtract :: ([Flags] -> Config) -> Config -- finish the process
myExtract builder = builder []

coMEx1 =
  let builder  = profile' defaultConfig
      builder' = optimize' builder
  in  myExtract builder'

-- generalize so we don't have to make these lambdas
myExtend :: (([Flags] -> Config) ->              Config ) --this is the function we want to make
       ->  ([Flags] -> Config) -> ([Flags] -> Config)   -- and it makes this one for us :)
myExtend configFn builder =
  \flgs2 -> configFn (\flgs1 -> builder $ flgs1 ++ flgs2)

-- If our datatype is monomorphic, then this is sufficient, we can sugar it up with some notation:
infixl 5 |>
(|>) :: a -> (a -> b) -> b
a |> f = f a

coMEx2 = defaultConfig |> profile' |> optimize' |> myExtract

--and likewise
infixl 5 ||>
(||>) :: ([Flags] -> Config)
     -> (([Flags] -> Config) -> Config)
     -> ([Flags] -> Config)
a ||> f = myExtend f a

coMEx3 = defaultConfig ||> profile ||> optimize |> myExtract


-- wouldn't it be nice if there was a typeclass to generalize this?
--   (In instances where we are using a polymorphic type?)
-- there is! yay!
-- import Control.Comonad and add comonad to depends
data Config2 a = MkConfig2 {c2 :: [a], dfltC :: a }
  deriving (Show)

defaultConfig2 :: [Flags] -> Config2 Flags
defaultConfig2 flags = MkConfig2 flags "-debug"

instance Functor (Config2) where
  fmap f (MkConfig2 a dflt) = MkConfig2 (map f a) (f dflt)

instance Comonad (Config2) where
  --extract :: (Config2 a) -> a
  extract (MkConfig2 [] dflt) = dflt
  extract (MkConfig2 (x:xs) dflt) = x
  --extend :: (Config2 a -> b) -> (Config2 a) -> b
  extend f ca = MkConfig2 [] (f ca)


profile2 :: ([Flags] -> Config2 Flags) -> Config2 Flags
profile2 builder = builder ["-a", "-debug"]

optimize2 :: ([Flags] -> Config2 Flags) -> Config2 Flags
optimize2 builder = builder ["-O2"]

infixl 5 |>>
(|>>) :: ([Flags] -> Config2 Flags)
     -> (([Flags] -> Config2 Flags) -> Config2 Flags)
     -> ([Flags] -> Config2 Flags)
a |>> f = extend f a

coMEx4 =
  defaultConfig2 |>> profile2 |>> optimize2 |> extract


-- Cofree
-- useful for annotating syntax trees, or something?
