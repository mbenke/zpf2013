# Rozgrzewka: wyrażenia regularne

Zadanie na wyrażenia regularne w podkatalogu Reg (opis w pliku zadanie.md)

# Typy jako język programowania

*    Funkcje na typach obliczane w czasie kompilacji

    ~~~~ {.haskell}
    data Zero
    data Succ n

    type One   = Succ Zero
    type Two   = Succ One
    type Three = Succ Two
    type Four  = Succ Three

    one   = undefined :: One
    two   = undefined :: Two
    three = undefined :: Three
    four  = undefined :: Four

    class Add a b c | a b -> c where
      add :: a -> b -> c
      add = undefined
    instance              Add  Zero    b  b
    instance Add a b c => Add (Succ a) b (Succ c)
    ~~~~ 

    ~~~~
    *Main> :t add three one
    add three one :: Succ (Succ (Succ (Succ Zero)))
    ~~~~

* Ćwiczenie: rozszerzyć o mnożenie i silnię

# Typy jako język programowania (2)
Wektory przy użyciu klas:

~~~~ {.haskell}
data Vec :: * -> * -> * where
  VNil :: Vec Zero a  
  (:>) :: a -> Vec n a -> Vec (Succ n) a

vhead :: Vec (Succ n) a -> a
vhead (x :> xs) = x
~~~~

**Ćwiczenie:** dopisać `vtail`, `vlast`

# Klasy wieloparametrowe

* Czasami potrzebujemy opisać nie tyle pojedynczy typ, co relacje między typami:

    ~~~~ {.haskell}
    {-#LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
    class Iso a b where
      iso :: a -> b
      osi :: b -> a
      
    instance Iso a a where
      iso = id
      osi = id

    instance Iso ((a,b)->c) (a->b->c) where
      iso = curry
      osi = uncurry

    instance (Iso a b) => Iso [a] [b] where
     iso = map iso
     osi = map osi
    ~~~~

* Uwaga: w ostatnim przykładzie `iso` ma inny typ po lewej, inny po prawej 

* Ćwiczenie: napisz jeszcze jakieś instancje klasy `Iso`


    ~~~~ {.haskell}
    instance (Functor f, Iso a b) => Iso (f a) (f b) where 
    instance Iso (a->b->c) (b->a->c) where
    ~~~~
