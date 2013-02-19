% Zaawansowane programowanie funkcyjne
% Marcin Benke
% 20 lutego 2013

<meta name="duration" content="80" />

# Plan wykładu
* Typy i klasy (ok. 3 wykładów)
    * Typy algebraiczne i klasy typów
    * Klasy konstruktorowe
    * Klasy wieloparametrowe, zależności funkcyjne
    * Rodziny typów, typy skojarzone, uogólnione typy algebraiczne (GADT)
* Typy zależne, Agda, Idris
* Metody kontrolowania efektów w języku funkcyjnym (ok. 6 wykładów)
    * Rodzaje efektów (błędy, stan, I/O, nawracanie)
    * Monady Error, State, IO, List
    * Studium biblioteki monadynej
    * Funktory aplikatywne i idiomy
    * Studium biblioteki idiomatycznej
<!--    * Strzałki -->
* Programowanie równoległe w Haskellu
    * Programowanie wielordzeniowe i wieloprocesorowe (SMP)
    * Równoległość danych (Data Parallel Haskell)
* Prezentacje projektów

Jakieś życzenia?

# Zasady zaliczania
* Laboratorium: projekt 1-3 osobowy.
* Egzamin ustny, którego istotną częścią jest prezentacja projektu.
* Alternatywna forma zaliczenia: referat (koniecznie ciekawy!)
* ...możliwe  także inne formy.

# Materiały

~~~~~
$ cabal install pandoc
$ PATH=~/.cabal/bin:$PATH            # Linux
$ PATH=~/Library/Haskell/bin:$PATH   # OS X
$ git clone git://github.com/mbenke/zpf2013.git
$ cd zpf2013/Slides
$ make
~~~~~

# języki funkcyjne
* typowane dynamicznie, gorliwe: Lisp
* typowane statycznie, gorliwe, nieczyste: ML
* typowane statycznie, leniwe, czyste: Haskell

Ten wykład: Haskell, ze szczególnym naciskiem na typy.

Bogata struktura typów jest tym, co wyróżnia Haskell wśród innych języków.

# Typy jako język specyfikacji

Typ funkcji często specyfikuje nie tylko jej wejście i wyjście ale i relacje między nimi:

~~~~ {.haskell}
f :: forall a. a -> a
f x = ?
~~~~

Jeśli `(f x)` daje wynik, to musi nim być `x`

* Philip Wadler "Theorems for Free"

* Funkcja typu `a -> IO b` może mieć efekty uboczne

    ~~~~ {.haskell}
    import Data.IORef

    f :: Int -> IO (IORef Int)
    f i = do
      print i
      r <- newIORef i
      return r

    main = do
      r <- f 42
      j <- readIORef r
      print j    
    ~~~~



# Typy jako język specyfikacji (2)

Funkcja typu `Integer -> Integer` zasadniczo nie może mieć efektów ubocznych

Liczby Fibonacciego w stałej pamięci

~~~~ {.haskell}
import Control.Monad.ST
import Data.STRef
fibST :: Integer -> Integer
fibST n = 
    if n < 2 then n else runST fib2 where
      fib2 =  do
        x <- newSTRef 0
        y <- newSTRef 1
        fib3 n x y
 
      fib3 0 x _ = readSTRef x
      fib3 n x y = do
              x' <- readSTRef x
              y' <- readSTRef y
              writeSTRef x y'
              writeSTRef y (x'+y')
              fib3 (n-1) x y
~~~~

Jak to?

~~~~
runST :: (forall s. ST s a) -> a
~~~~

Typ `runST` gwarantuje, że efekty uboczne nie wyciekają. Funkcja `fibST`
jest czysta.

# Typy jako język projektowania

* Projektowanie programu przy użyciu typów i `undefined`

    ~~~~ {.haskell}
    conquer :: [Foo] -> [Bar]
    conquer fs = concatMap step fs

    step :: Foo -> [Bar]
    step = undefined
    ~~~~

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

Chcielibyśmy również mieć

~~~~ {.haskell}
vappend :: Add m n s => Vec m a -> Vec n a -> Vec s a
~~~~

ale tu niestety system typów okazuje się za słaby

# Typy jako język programowania (3)

* Wektory przy użyciu rodzin typów:

    ~~~~ {.haskell}
    data Zero = Zero
    data Suc n = Suc n

    type family m :+ n
    type instance Zero :+ n = n
    type instance (Suc m) :+ n = Suc(m:+n)

    data Vec :: * -> * -> * where
      VNil :: Vec Zero a  
      (:>) :: a -> Vec n a -> Vec (Suc n) a

    vhead :: Vec (Suc n) a -> a
    vappend :: Vec m a -> Vec n a -> Vec (m:+n) a
    ~~~~

* Sprytna sztuczka o wątpliwej wartości praktycznej

# Typy zależne

Prawdziwe programowanie na poziomie typów  i dowodzenie własności programów możliwe w języku z typami zależnymi, takim jak Agda, Epigram, Idris

~~~~
module Data.Vec where
infixr 5 _∷_

data Vec (A : Set a) : ℕ → Set where
  []  : Vec A zero
  _∷_ : ∀ {n} (x : A) (xs : Vec A n) → Vec A (suc n)

_++_ : ∀ {a m n} {A : Set a} → Vec A m → Vec A n → Vec A (m + n)
[]       ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

module UsingVectorEquality {s₁ s₂} (S : Setoid s₁ s₂) where
  xs++[]=xs : ∀ {n} (xs : Vec A n) → xs ++ [] ≈ xs
  xs++[]=xs []       = []-cong
  xs++[]=xs (x ∷ xs) = SS.refl ∷-cong xs++[]=xs xs
~~~~


# Problem z typami zależnymi

O ile Haskell bywa czasami nieczytelny, to z typami zależnymi całkiem łatwo przesadzić:

~~~~
  now-or-never : Reflexive _∼_ →
                 ∀ {k} (x : A ⊥) →
                 ¬ ¬ ((∃ λ y → x ⇓[ other k ] y) ⊎ x ⇑[ other k ])
  now-or-never refl x = helper <$> excluded-middle
    where
    open RawMonad ¬¬-Monad

    not-now-is-never : (x : A ⊥) → (∄ λ y → x ≳ now y) → x ≳ never
    not-now-is-never (now x)   hyp with hyp (, now refl)
    ... | ()
    not-now-is-never (later x) hyp =
      later (♯ not-now-is-never (♭ x) (hyp ∘ Prod.map id laterˡ))

    helper : Dec (∃ λ y → x ≳ now y) → _
    helper (yes ≳now) = inj₁ $ Prod.map id ≳⇒ ≳now
    helper (no  ≵now) = inj₂ $ ≳⇒ $ not-now-is-never x ≵now
~~~~

...chociaż oczywiście pisanie takich dowodów jest świetną zabawą.


# Data Parallel Haskell

Dokąd chcemy dojść: 

~~~~ {.haskell}
{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS_GHC -fvectorise #-}

module DotP where
import qualified Prelude
import Data.Array.Parallel
import Data.Array.Parallel.Prelude
import Data.Array.Parallel.Prelude.Double as D

dotp_double :: [:Double:] -> [:Double:] -> Double
dotp_double xs ys = D.sumP [:x * y | x <- xs | y <- ys:]
~~~~

Wygląda jak operacja na listach, ale działa na tablicach i
"automagicznie" zrównolegla się na dowolną liczbę rdzeni/procesorów
(także CUDA).

Po drodze czeka nas jednak trochę pracy.

# Typy w Haskellu

* typy bazowe: `zeroInt :: Int`
* typy funkcyjne: `plusInt :: Int -> Int -> Int`
* typy polimorficzne `id :: a -> a`

    ~~~~ {.haskell}
    {-# LANGUAGE ExplicitForAll #-}
    g :: forall b.b -> b
    ~~~~

* typy algebraiczne 

    ~~~~ {.haskell}
    data Tree a = Leaf | Node a (Tree a) (Tree a)
    ~~~~

* `Leaf` i `Node` są konstruktorami wartości: 

    ~~~~ {.haskell}
    data Tree a where
    	 Leaf :: Tree a
         Node :: a -> Tree a -> Tree a -> Tree a
    ~~~~

* `Tree` jest *konstruktorem typowym*, czyli operacją na typach

* NB od niedawna Haskell dopuszcza puste typy:

    ~~~~ {.haskell}
    data Zero
    ~~~~
  
# Typowanie polimorficzne

* Generalizacja:

$${\Gamma \vdash e :: t, a \notin FV( \Gamma )}\over {\Gamma \vdash e :: \forall a.t}$$

 <!-- 
Jeśli $\Gamma \vdash e :: t, a \notin FV( \Gamma )$
 
to $\Gamma \vdash e :: \forall a.t$

  Γ ⊢ e :: t, a∉FV(Γ)
$$\Gamma \vdash e :: t$$ ,
 \(a \not\in FV(\Gamma) \) , 
to $\Gamma \vdash e :: \forall a.t$
-->

Na przykład

$${ { \vdash map :: (a\to b) \to [a] \to [b] } \over
   { \vdash map :: \forall b. (a\to b) \to [a] \to [b] } } \over
   { \vdash map :: \forall a. \forall b. (a\to b) \to [a] \to [b] } $$

Uwaga:

$$ f : a \to b \not \vdash map\; f :: \forall b. [a] \to [b]  $$

* Instancjacja

$$ {\Gamma \vdash e :: \forall a.t}\over {\Gamma \vdash e :: t[a:=s]} $$
 
# Klasy

* klasy opisują własności typów

    ~~~~ {.haskell}
    class Eq a where
      (==) :: a -> a -> Bool
    instance Eq Bool where
       True  == True  = True
       False == False = True
       _     == _     = False
    ~~~~
    
* funkcje mogą być definiowane w kontekście klas:

    ~~~~ {.haskell}
    elem :: Eq a => a -> [a] -> Bool
    ~~~~

+ Implementacja 
    - instancja tłumaczona na słownik metod (coś \'a la  vtable w C++)
    - kontekst (np Eq a) jest tłumaczony na ukryty parametr (słownik metod )
    - podklasa tłumaczona na funkcję

Patrz `Code/Class0/B*`

# B.hs

~~~~ {.haskell}
module B where
data B = F | T

n :: B -> B
n T = F
n F = T

class E a where
  eq :: a -> a -> B
  
instance E B where 
  eq x y = case x of
    T -> y
    F -> case y of 
      T -> F
      F -> T

neq :: E a => a -> a -> B
neq x y = n (eq x y)
~~~~~

Tłumaczenie na Haskell Core mozemy uzyskać przez

~~~~ 
ghc -c -fext-core B.hs
~~~~

# Klasy w Haskell Core

~~~~
  %data B =
    {F;
     T};
  %newtype DicE CoDE a
    = a -> a -> B;

-- DicE a to slownik metod
-- CoDE :: (DicE a) ~ (a -> a -> B) -- koercja
-- newtype jest tylko optymalizacja (bo tylko jedna metoda)
-- w gruncie rzeczy mogłoby być 
--   %data DicE
-- eq to selektor metody ma typ mniej wiecej taki:

-- forall a. (DicE a) -> a -> a -> B
  eq :: %forall a . (DicE a) -> a -> a -> B =
    \ @ a (d::(DicE a)) ->
        %cast d
        (CoDE a); -- wyjęcie metody typu (a -> a -> B) ze słownika d

-- n jest "zwykla funkcja"
  n :: B -> B =
    \ (dsdc0::B) ->
        %case B dsdc0 %of {F -> T; T -> F};

-- neq :: forall a. (dE :: slownikE a) -> a -> a -> B
-- n, eq rozwiniete
  neq :: %forall a . (DicE a) -> a -> a -> B =
    \ @ a0  -- a0 jest typem (tzw "duża lambda", Λa.M)
      (dicE::(DicE a0))
      (x::a0)
      (y::a0) ->
-- case m_eq dicE x y :: B of
        %case B ((%cast (dicE)
                         ((CoDE a0)))
                        x y)  
        %of {F -> T; T -> F};

-- tu jest kod implementacji metody eq dla B
  zdceqrc7 :: B -> B -> B =
    \ (xabB::B) (yabC::B) ->
        %case B xabB %of (wildX9::B)
          {F ->
             %case B yabC %of (wild1X6::B)
               {F ->
                  T;
                T ->
                  F};
           T ->
             yabC};

-- tu jest slownik metod klasy E dla typu B
  zdfEB :: (DicE B) =
    %cast (zdceqrc7)
    (%sym ((CoDE B)));
~~~~

# Podklasy (konteksty klas)

* Jeśli typ a jest klasy C i są zdefiniowane funkcje ... to jest klasy D

    ~~~~ {.haskell}
    class Eq a => Ord a where
      compare :: a -> a -> Ordering
      (<) :: a -> a -> Bool
      ...
    ~~~~

* Oczywiście mają niewiele wspólnego z podklasami obiektowymi, ale używa się takiego skrótu. Prawdopodobnie lepiej myśleć o kontekstach klas.

* Graf podklas musi być acykliczny

* Specyfikowanie nadklas jest kwestią smaku - można dyskutować, 
czy `Eq` rzeczywiście jest niezbędne dla `Ord`, albo czy każda instancja `Monad` musi być instancją `Functor`.

Patrz `Code/Trivia/C*`

# C.hs

~~~~ {.haskell}
module C where

class C1 a where
  m0 :: a 
  m1 :: a -> a
  
class C1 a => C2 a where
  m2 :: a -> a -> a
~~~~

# C.hcr

~~~~ {.haskell}
%module main:C
  %data TZCC1 aabv =
    {DZCC1 aabv (aabv -> aabv)};
  %data TZCC2 aabu =
    {DZCC2 ((TZCC1 aabu)) (aabu -> aabu -> aabu)};
  zdp1C2 :: %forall aabu . (TZCC2 aabu) ->
                                  (TZCC1 aabu) =
    \ @ aabu (tplB1::(TZCC2 aabu)) ->
        %case ((TZCC1 aabu)) tplB1 %of (tplX4::(TZCC2 aabu))
          {DZCC2
           (tplB2::(TZCC1 aabu)) (tplB3::aabu -> aabu -> aabu) ->
             tplB2};
  m2 :: %forall aabu . (TZCC2 aabu) ->
                              aabu -> aabu -> aabu =
    \ @ aabu (tplB1::(TZCC2 aabu)) ->
        %case (aabu -> aabu -> aabu) tplB1 %of (tplX4::(TZCC2 aabu))
          {DZCC2
           (tplB2::(TZCC1 aabu)) (tplB3::aabu -> aabu -> aabu) ->
             tplB3};
  m0 :: %forall aabv . (TZCC1 aabv) -> aabv =
    \ @ aabv (tplB1::(TZCC1 aabv)) ->
        %case aabv tplB1 %of (tplX4::(TZCC1 aabv))
          {DZCC1 (tplB2::aabv) (tplB3::aabv -> aabv) ->
             tplB2};
  m1 :: %forall aabv . (TZCC1 aabv) -> aabv -> aabv =
    \ @ aabv (tplB1::(TZCC1 aabv)) ->
        %case (aabv -> aabv) tplB1 %of (tplX4::(TZCC1 aabv))
          {DZCC1 (tplB2::aabv) (tplB3::aabv -> aabv) ->
             tplB3};
~~~~

