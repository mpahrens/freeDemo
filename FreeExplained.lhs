Clarifying Free Monads.

Free is a type with kind (* -> *) -> * -> *
Meaning it wants two things:
1) Some sort of structure with kind (*->*)
2) Some actualized type with kind *

We can see this with the righthand side of the datatype declaration.
> data Free f a = Pure a | Free (f (Free f a))
where :k f :: * -> *
      :k a :: *

Compare this to the datatype for list
> data List a = Nil | Cons a (List a)

compare Free, the data constructor and cons (in a GADT like syntax)
> Free :: f (Free f a) -> Free f a
> Cons :: a -> List a -> List a

imagine:
> data List' a = Free ((,) a) ()
So
> List a = Free((,) a) ()
> = Free (a, List' a)) | Pure ()
> = Free a (List' a) | Pure ()
> ~ Cons' a (List' a) | Nil'
