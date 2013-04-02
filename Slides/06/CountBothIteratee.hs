import Prelude hiding (take)
import IterateeM
import Data.Char (isSpace)

main = runCountSpacesAndTheAtFirstNChars 100 "iteratee.md"

runCountSpacesAndTheAtFirstNChars n fileName =
  print =<< run =<< enum_file fileName .|
    take n (countSpaces `en_pair` countThe)

countSpaces :: Monad m => Iteratee Char m Int
countSpaces = id .| (en_filter isSpace) count_i

countThe :: Monad m => Iteratee Char m Int
countThe = id .| enum_words .| en_filter (== "the") count_i

-- Count the stream
-- This iteratee does not do any IO (betrayed by its type, polymorphic
-- over the monad m). The counting iteratee is also polymorphic over stream
-- elements: it can count any streams. 

count_i :: Monad m => Iteratee el m Int
count_i = ie_cont $ step 0
  where
  step acc (Chunk [])  = ie_contM (step acc) 
  step acc (Chunk [_]) = ie_contM (step $! succ acc)
  step acc (Chunk ls)  = ie_contM (step $! acc + length ls)
  step acc stream      = ie_doneM acc stream

-- Like a combination of head and peek
-- (Some Iteratee libraries may provide this function)
getchar :: Monad m => Iteratee el m (Maybe el)
getchar = headM

en_filter :: Monad m => (el -> Bool) -> Enumeratee el el m a
en_filter test i@(IE_cont Nothing k) = ie_cont step
  where
  step (Chunk l) =
    case filter test l of
      [] -> ie_contM step
      l  -> feedI k (Chunk l) >>= \i ->
        return (en_filter test i, empty_stream)
  step s = ie_doneM i s
en_filter _ i = return i

-- Parallel composition of Iteratees
-- The following enumeratee enum2' applies two iteratees to the same stream,
-- in effect, splitting the stream in two.
-- There is no buffering however: enum2' receives a chunk and passes
-- it to both iteratees, before asking for the next chunk.
-- The enumeratee enum2' finishes either when the stream is over or
-- when at least one of the given iteratees finish. 
-- The IterateeM.hs library has the similar combinator enum2, 
-- which continues requesting stream data so long as at least 
-- one of the iteratees wants them.

enum2' :: Monad m => Iteratee el m a -> Iteratee el m b ->
      Iteratee el m (Iteratee el m a, Iteratee el m b)
enum2' (IE_cont Nothing k1) (IE_cont Nothing k2) = ie_cont (step k1 k2)
  where
  step k1 k2 (Chunk [])  = ie_contM (step k1 k2)
  step k1 k2 str@Chunk{} = feedI k1 str >>= \i1 ->
         feedI k2 str >>= \i2 ->
         ie_ret $ enum2' i1 i2
  step k1 k2 str         = ie_doneM (ie_cont k1, ie_cont k2) str
 -- at least one iteratee is finished and doesn't want any data
enum2' i1 i2 = return (i1,i2)

-- A convenient combinator for the frequently occurring pattern
en_pair :: Monad m => Iteratee el m a -> Iteratee el m b ->
      Iteratee el m (a,b)
en_pair i1 i2 = enum2' i1 i2 >>= runI2
