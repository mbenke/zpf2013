import IterateeM
import Data.Char(isSpace)

runCountSpaces fileName = print =<< run =<< enum_file fileName countSpaces

main = runCountSpaces  "Makefile"

countSpaces :: Monad m => Iteratee Char m Int
countSpaces = id .| (en_filter isSpace) count_i

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
 step (Chunk l) = case filter test l of
                   [] -> ie_contM step
		   l  -> feedI k (Chunk l) >>= \i ->
			 return (en_filter test i, empty_stream)
 step s         = ie_doneM i s
en_filter _ i   = return i