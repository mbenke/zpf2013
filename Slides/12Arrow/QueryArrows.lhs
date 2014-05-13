Torsten and Niklas have suggested using Haskell's list comprehensions
for doing relational algebra-like manipulations in Haskell.  I'll give
you a concrete example along similar lines, except it uses arrows in
place of monads.

The reason I chose arrows here is that I have a Haskell embedded
relational query DSL that compiles to SQL and it seems that due to the
limits of expressivity of SQL one must use arrows there.  The API I
present below is almost a direct translation of my relational query
API.

What I've written here could be done with monads, of course, but by
the same "principle of least power" that tells me not to use 'do'
notation and monads where applicatives would suffice, I'll do this in
terms of arrows.  It will also serve as a neat introduction for anyone
who's interested in learning my relational query DSL.

> {-# LANGUAGE Arrows #-}
> 
> import Control.Arrow (Kleisli(Kleisli), runKleisli, returnA, arr, first,
>                       (<<<))

We're going to be working with lists of "records".  Here the records
are just tuples, but you could use Haskell record syntax if you like.

The important type is 'a -> [b]' and to make it an arrow we wrap it
with 'Kleisli'.  Recall that 'Kleisli m a b' is just 'a -> m b'.

> type QueryArr = Kleisli []
> type Query = QueryArr ()

So a 'Query a' is just a (wrapped) '[a]'.  'fromList' is a trivial
function that does the conversion and 'display' goes the other way to
show the result of queries.

> fromList :: [a] -> Query a
> fromList = Kleisli . const
>
> display :: Query String -> IO ()
> display = mapM_ putStrLn . flip runKleisli ()

Now let's define a "table" of people

> newtype PersonId = PersonId Int deriving Eq
> 
> people :: Query (PersonId, String)
> people = (fromList . map (first PersonId)) [ (1, "Tom")
>                                            , (2, "Duncan")
>                                            , (3, "Simon") ]

and a mapping from people to their favourite feature of Haskell.

> favouriteFeature :: Query (PersonId, String)
> favouriteFeature = (fromList . map (first PersonId)) [ (1, "arrows")
>                                                      , (2, "cabal")
>                                                      , (3, "purity") ]

We'd like to join 'people' to 'favouriteFeature' by requiring the
"person ID" columns to be equal.  For that we need two
combinators. 'eq' just checks whether two columns are equal

> eq :: Eq a => QueryArr (a, a) Bool
> eq = Kleisli (\(x, y) -> [x == y])

and 'restrict' restricts the query to the case where its argument is
'True'.

> restrict :: QueryArr Bool ()
> restrict = Kleisli guard
>   where guard = \cond -> if cond then [()] else []
>         -- This is exactly Control.Monad.guard for []

Then we can write joins straightforwardly using arrow notation (which
is rather similar to 'do' notation).

> favourites :: Query String
> favourites = proc () -> do
>   -- Corresponding to SQL's 'FROM ...'
>   (pid, name) <- people -< ()
>   (pid', feature) <- favouriteFeature -< ()
> 
>   -- Corresponding to SQL's 'WHERE ... = ...'
>   restrict <<< eq -< (pid, pid')
> 
>   -- Corresponding to SQL's 'SELECT ...'
>   returnA -< name ++ "'s favourite feature is " ++ feature

ghci> display favourites
Tom's favourite feature is arrows
Duncan's favourite feature is cabal
Simon's favourite feature is purity

We can do joins on any conditions of course, not just equality.
Here's a greater-than-or-equal-to predicate.

> gte :: (Ord a, Eq a) => QueryArr (a, a) Bool
> gte = Kleisli (\(x, y) -> [x >= y])

We can use it by comparing a person's skill level to that required to
perform certain tasks.

A person has this skill level

> skillLevel :: Query (PersonId, Int)
> skillLevel = (fromList . map (first PersonId)) [ (1, 5)
>                                                , (2, 9)
>                                                , (3, 10) ]

and a role requires this skill level.

> roleDifficulties :: Query (Int, String)
> roleDifficulties = fromList [ (3, "write foldr")
>                             , (8, "implement stream fusion")
>                             , (10, "be god") ]

A person thus has these abilities.

> abilities :: Query String
> abilities = proc () -> do
>   (pid, name) <- people -< ()
>   (pid', skill) <- skillLevel -< ()
>   (requiredSkill, role) <- roleDifficulties -< ()
> 
>   restrict <<< eq -< (pid, pid')
>   restrict <<< gte -< (skill, requiredSkill)
> 
>   returnA -< name ++ " can " ++ role

*Main> display abilities
Tom can write foldr
Duncan can write foldr
Duncan can implement stream fusion
Simon can write foldr
Simon can implement stream fusion
Simon can be god

So far what we have is a somewhat awkward encoding of what we would
have written in SQL.  Where Haskell really shines, as usual, is in its
power to abstract.  I'll show you one of the abstractions now, which
is to let queries have inputs as well as ouputs.

We can write 'skillLevelOfPerson' which maps a 'PersonId' to that
person's skill level.

> skillLevelOfPerson :: QueryArr PersonId Int
> skillLevelOfPerson = proc pid -> do
>   (pid', skill) <- skillLevel -< ()
>   restrict <<< eq -< (pid, pid')
>   returnA -< skill

In 'skillLevelOfPerson' the output depends functionally on the input,
but in general this needn't hold.  'rolesOfSkillLevel' maps a skill
level to the zero or more roles available at that skill level.

> rolesOfSkillLevel :: QueryArr Int String
> rolesOfSkillLevel = proc skill -> do
>   (requiredSkill, role) <- roleDifficulties -< ()
>   restrict <<< gte -< (skill, requiredSkill)
>   returnA -< role

The great benefit of abstracting out these two query arrows is that
everything becomes much more composable.  For example, we can now
create a query arrow to map a person to the roles available to them

> rolesOfPerson :: QueryArr PersonId String
> rolesOfPerson = rolesOfSkillLevel <<< skillLevelOfPerson

and use it reimplement 'abilities'

> abilities' :: Query String
> abilities' = proc () -> do
>   (pid, name) <- people -< ()
>   role <- rolesOfPerson -< pid
>   returnA -< name ++ " can " ++ role

*Main> display abilities'
Tom can write foldr
Duncan can write foldr
Duncan can implement stream fusion
Simon can write foldr
Simon can implement stream fusion
Simon can be god

Once everything is written in this composable way, you may or may not
find it more convenient to use arrow combinators directly.

> abilities'' :: Query String
> abilities'' =  arr (\(role, name) -> name ++ " can " ++ role)
>                <<< first rolesOfPerson
>                <<< people

*Main> display abilities''
Tom can write foldr
Duncan can write foldr
Duncan can implement stream fusion
Simon can write foldr
Simon can implement stream fusion
Simon can be god


Be careful using this implementation: every time you bring another
"table" into scope you will do a linear scan over it.  Thus the time
complexity of running queries is exponential in the number of
"tables"!  If one wanted, one could use the same API with a much more
intelligent query planning engine underneath.

As I mentioned above, I have a Haskell relational query EDSL (called
Opaleye) which has almost exactly this API and compiles to SQL.  If
anyone is interested in a type safe, composable relational query API
please let me know and I'll discuss how you can get access (whilst in
development it is not yet public).

You can see the slides from a presentation I gave about it at
Netherland FP Day 2014 here:

    http://staff.science.uva.nl/~grelck/nl-fp-day-2014.html

Tom
