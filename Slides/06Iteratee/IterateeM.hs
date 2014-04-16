-- Haskell98!

-- Monadic and General Iteratees:
-- incremental input parsers, processors and transformers

-- The running example, parts 1 and 2
-- Part 1 is reading the headers, the sequence of lines terminated by an
-- empty line. Each line is terminated by CR, LF, or CRLF.
-- We should return the headers in order. In the case of error,
-- we should return the headers read so far and the description of the error.
-- Part 2 is reading the headers and reading all the lines from the
-- HTTP-chunk-encoded content that follows the headers. Part 2 thus
-- verifies layering of streams, and processing of one stream
-- embedded (chunk encoded) into another stream.

module IterateeM where

import System.Posix
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Data.List (splitAt)
import Prelude hiding (head, drop, dropWhile, take, break, catch)
import qualified Prelude
import Data.Char (isHexDigit, digitToInt, isSpace)
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Identity
import Control.Exception
import Data.Typeable

import LowLevelIO

-- A stream is a (continuing) sequence of elements bundled in Chunks.
-- The first variant means no more data will be coming: the stream
-- is exhausted, either due to EOF or some error.
-- Chunk [a] gives the currently available part of the stream.
-- The stream is not terminated yet.
-- The case (Chunk []) signifies a stream with no currently available
-- data but which is still continuing. A stream processor should,
-- informally speaking, ``suspend itself'' and wait for more data
-- to arrive.
type ErrMsg = SomeException
data Stream el = EOF (Maybe ErrMsg) | Chunk [el] deriving Show


-- Iteratee -- a generic stream processor, what is being folded over
-- a stream
-- This is the monadic version of Iteratee from Iteratee.hs
-- Please see the file Iteratee.hs for the discussion
-- of design decisions.
-- Iteratee exists in one of the three states:
--    -- Done state: the iteratee has computed the result
--       and doesn't want any more data
--    -- More data state, holding the continuation (the `step function')
--       to process a chunk of data and yield a new state
--    -- A message to the stream producer (e.g., to rewind the stream)
--       or an error indication.
-- We assume that all iteratees are `good' -- given bounded input,
-- they do the bounded amount of computation and take the bounded amount
-- of resources.
-- We also assume that given a terminated stream, an iteratee
-- moves to the done state, so the results computed so far could be returned.
-- The monad m describes the sort of computations done
-- by the iteratee as it processes the stream. The monad m could be
-- the identity monad (for pure computations) or the IO monad
-- (to let the iteratee store the stream processing results as they
-- are computed).



-- We have investigated three design choices. The following is the latest
-- one

data Iteratee el m a = IE_done ! a
		     | IE_cont ! (Maybe ErrMsg)
		               (Stream el -> m (Iteratee el m a, Stream el))
		               

-- It is quite obvious that the second argument of IE_cont is
-- StateT (Stream el) m (Iteratee el m a)
-- To properly abstract from the stream and the mechanism of state
-- passing, we should have written
-- data Iteratee m a = IE_done a |
--  IE_cont (Maybe ErrMsg) (forall t. MonadStreamT t => t m (Iteratee m a)) 
-- where MonadStreamT is a variation of a monad state transformer,
-- with methods to get the current element of the stream and advance
-- the stream; to peek at the current chunk (without advancing the stream),
-- to advance the stream and to get and set an error.
-- The more general type in the comment makes it clear that Iteratee
-- cannot `leak' the state (e.g., IO buffers) of the enumerator:
-- when the enumerator is finished, all the state is disposed of and none
-- of it leaks outside.

{-
The current design enforces that the Iteratee returned by any
enumerator or enumeratee is either in the Cont state, or the Done
state with no stream remaining. The stream produced by enumerator
cannot `leak'.

Now, it is almost always that
        enum (i1 >> i2) /= enum i1 >> enum i2
  
But that is hardly surprising, since we already have
        atomically (m1 >> m2) /= atomically m1 >> atomically m2
        lift (m1 >> m2) /= lift m1 >> lift m2
  
and, purely,
        round (x1 + x2) /= round x1 + round x2
  
Incidentally, there is quite a bit of similarity between transactions
and nested streams; in
        take n iter1 >> take m iter2
the iteratee iter2 is supposed to be isolated from iter1; iter1 can
read as much as it wants or as little as it wants, and returning all
sorts of errors. We will still read exactly n elements from the outer
stream (if there are that many). It is quite common to package for
transmission several pieces of data by prefixing the data with their
length and concatenating the results. Since the pieces of data could
be produced separately and consumed separately, an error in processing
one piece should not automatically affect the processing of the rest.
If we process data incrementally, as we read it from a socket or a
serial port, it is imperative that we always read the complete frame
of data (regardless of how successfully that data was
processed). Otherwise, we deadlock.
-}

-- It turns out, Iteratee forms a monad. We can use the familiar do
-- notation for composing Iteratees
instance Monad m => Monad (Iteratee el m) where
    {-# INLINE return #-}
    return = IE_done

    {-# INLINE (>>=) #-}
    IE_done a   >>= f = f a
    IE_cont e k >>= f = IE_cont e (\s -> k s >>= docase)
     where
     docase (IE_done a, stream)   = case f a of
		   IE_cont Nothing k -> k stream
		   i                 -> return (i,stream)
     docase (i, s)  = return (i >>= f, s)

    fail = throwErrStr

instance MonadTrans (Iteratee el) where
    {-# INLINE lift #-}
    lift m = IE_cont Nothing (\s -> m >>= \v -> return (return v,s))

instance MonadIO m => MonadIO (Iteratee el m) where
    liftIO = lift . liftIO

-- Throw an irrecoverable error
throwErr :: Monad m => ErrMsg -> Iteratee el m a
throwErr e = IE_cont (Just e) (\s -> return (throwErr e, s))

throwErrStr :: Monad m => String -> Iteratee el m a
throwErrStr msg = throwErr (toException (ErrorCall msg))

-- Throw a recoverable error
throwRecoverableErr :: Monad m => ErrMsg
		    -> (Stream el -> m (Iteratee el m a, Stream el))
		    -> Iteratee el m a
throwRecoverableErr e i = IE_cont (Just e) i


-- Produce the EOF error message to be passed to throwErr. 
-- If the stream was terminated because of an error, keep the original 
-- error message.
setEOF :: Stream el -> ErrMsg
setEOF (EOF (Just e)) = e
setEOF _              = exc_EOF

-- Common errors
exc_EOF       = toException $ ErrorCall "EOF"
exc_divergent = toException $ ErrorCall "divergent iteratee"
exc_IOErr (Errno errno) = toException . ErrorCall $
			  "IO Error, errno " ++ show errno


-- Useful combinators for implementing iteratees and enumerators

empty_stream :: Stream el
empty_stream = Chunk []

ie_cont :: Monad m => (Stream el -> m (Iteratee el m a,Stream el)) ->
	              Iteratee el m a
ie_cont = IE_cont Nothing

{-# INLINE ie_doneM #-}
ie_doneM :: Monad m => a -> Stream el -> m (Iteratee el m a, Stream el)
ie_doneM x s = return (IE_done x, s)

{-# INLINE ie_contM #-}
ie_contM :: Monad m => (Stream el -> m (Iteratee el m a,Stream el)) ->
	    m (Iteratee el m a,Stream el)
ie_contM k = return (ie_cont k, empty_stream)

-- ie_ret often occurs when writing step functions of iteratees
{-# INLINE ie_ret #-}
ie_ret :: Monad m => Iteratee el m a -> m (Iteratee el m a, Stream el)
ie_ret i = return (i,empty_stream)

-- As a matter of fact, liftI always occurs in the the following pattern
--     liftI (feedI k something >>= ...)
-- in enumeratees.
-- Therefore, it is worth capture that pattern instead.
-- Our bindI is a composition of lift and bind, optimized by inlining.
-- {-# INLINE bindI #-}
-- bindI :: Monad m => m a -> (a -> Iteratee el m b) -> Iteratee el m b
-- bindI m f = ie_cont (\s -> m >>= run' s . f)
--  where 
--  run' s (IE_cont Nothing k) = k s
--  run' s i                   = return (i,s)


-- Feed Iteratee a piece of stream. Disregard the remaining stream
-- (the operation typically used by enumerators)
{-# INLINE feedI #-}
feedI :: Monad m =>
	 (Stream el -> m (Iteratee el m a, Stream el)) ->
	 Stream el ->
	 m (Iteratee el m a)
feedI k str = k str >>= return . fst

-- Send EOF to Iteratee and disregard the unconsumed part of the stream
run :: Monad m => Iteratee el m a -> m a
run (IE_done x)         = return x
run (IE_cont Nothing k) = k (EOF Nothing) >>= check
 where
 check (IE_done x, _)  = return x
 check (IE_cont e _,_) = error $ "control message: " ++ show e
run (IE_cont (Just e) m) = error $ "control message: " ++ show e

-- The following is a variant of `run' which returns the result
-- of the (Iteratee el' m a) in the monad that is (Iteratee el m).
-- This variant embodies the common pattern of running the embedded
-- iteratee (embedded by an enumeratee):
--   do
--     lines <- runI =<< enum_lines stream2list
-- The tests below show many examples (e.g., read_lines_and_one_more_line).
--
-- runI can be implemented as lift . run
-- The following is an optimized implementation, obtained by inlining.
runI :: Monad m => Iteratee el' m a -> Iteratee el m a
runI (IE_done x)          = return x
runI (IE_cont Nothing k)  = ie_cont $ (k (EOF Nothing) >>=) . check
 where
 check s (IE_done x,_)    = return (return x, s)
 check s (IE_cont e _,_)  = return (throwErr (maybe exc_divergent id e),s)
runI (IE_cont (Just e) _) = throwErr e

-- Freequently we do not wish to do anything with the outer stream
-- after the inner iteratee is finished. 
-- This is the case of stream filters, or Unix pipes
-- We define a convenient abbreviation for that
{-# INLINE (.|) #-}
infixr 1 .|
(.|) :: Monad m => (Iteratee el m a -> w) -> Iteratee el m (Iteratee el' m a)
	-> w
enor .| enee = enor (runI =<< enee)

-- Later on, for optimization, consider CPS. It can avoid a few construction/
-- destruction operations. See IterateeCPS.hs

{-
The second design:

newtype Iteratee el m a = Iteratee{runIter:: m (IterV el m a)}
data IterV el m a = IE_done a (Stream el)
		  | IE_cont (Stream el -> Iteratee el m a) (Maybe ErrMsg)

instance Monad m => Monad (Iteratee el m) where
    return x = ie_done x (Chunk [])

    m >>= f = Iteratee $ runIter m >>= docase
     where
     docase (IE_done a (Chunk [])) = runIter (f a)
     docase (IE_done a stream)     = runIter (f a) >>= 
            \r -> case r of
		   IE_done x _         -> return $ IE_done x stream
		   IE_cont k Nothing   -> runIter $ k stream
		   iv                  -> return iv
     docase (IE_cont k e) = return $ IE_cont ((>>= f) . k) e

The design did not statically prevent leaking of enumerator's resources.
In fact, an enumeratee map_stream did leak the state of the enumerator
as the remaining-stream part of the IE_done value. Here is the problematic
example by Ben Lee explained by John Lato:

Prelude Data.Iteratee> run =<< (enumPureNChunk [1..10] 2 $ (runI =<<
map_stream id (Data.Iteratee.drop 1)) >> Data.Iteratee.head)
3
Prelude Data.Iteratee> run =<< (enumPureNChunk [1..10] 4 $ (runI =<<
map_stream id (Data.Iteratee.drop 1)) >> Data.Iteratee.head)
5

The problem is that map_stream failed to enforce the `transaction boundaries':
We should treat each enumerator/enumeratee as `atomically'.
After the processing of the stream supported by the enumerator is finished,
all enumerator/stream resources are disposed of.
-}

{-
-- The third design, from Iteratee.hs. It seems worse as we discuss
-- below.

newtype Iteratee el m a = Iteratee{runIter:: Stream el -> m (IterV el m a)}
data IterV el m a = IE_done a (Stream el) 
		  | IE_cont (Iteratee el m a) (Maybe ErrMsg)

type IterateePure el m a = Stream el -> IterV el m a

-- It turns out, Iteratee forms a monad. We can use the familiar do
-- notation for composing Iteratees
instance Monad m => Monad (Iteratee el m) where
    return x = Iteratee (return . IE_done x)
    Iteratee m >>= f = Iteratee $ \s -> m s >>= \r -> 
                            case r of
				IE_done x s -> runIter (f x) s
				IE_cont m e -> return $ IE_cont (m >>= f) e

instance MonadTrans (Iteratee el) where
    lift m = Iteratee (\s -> m >>= \x -> return $ IE_done x s)

-- On the up side, Iteratee looks like a IO monad: Stream acts
-- like theWorld....

-- Drawbacks of this encoding:
--  -- an Iteratee may already be in the done or the error state.
--  We won't know until we feed it some data (be it Chunk []).
--
-- The biggest problem with this encoding becomes apparent only
-- when we start implementing enumerators or enumeratees. We take an iteratee,
-- feed it some data. It returns (IE_done x rest). Now, the enumerator has
-- to return an iteratee, that is, a function
--        \stream -> IE_done x ???
-- What to put in place of ??? then? It should be something like
-- rest ++ stream. However, a particular stream may not necessarily
-- support concatenation (for example, consider a stream to be a pointer
-- to a shared IO buffer, to be used linearly). 
-- Concatenation is also wasteful, requiring allocation or holding onto
-- the previous data. In some circumstances
-- (see `take' below) we can disregard `rest'. But generally we can't.

-- Throw an unrecoverable error
throwErr :: Monad m => ErrMsg -> Iteratee el m a
throwErr e = Iteratee (\_ -> return $ IE_cont (throwErr e) (Just e))

-- Throw a recoverable error
throwRecoverableErr :: Monad m => ErrMsg -> Iteratee el m a -> Iteratee el m a
throwRecoverableErr e i = Iteratee (\_ -> return $ IE_cont i (Just e))

-- Send EOF to Iteratee and disregard the unconsumed part of the stream
run :: Monad m => Iteratee el m a -> m a
run iter = runIter iter (EOF Nothing) >>= \r -> 
	    case r of 
	     IE_done x _ -> return x
	     IE_cont _ e -> error $ "control message: " ++ show e

-- Useful combinators for implementing iteratees and enumerators

liftI :: Monad m => IterateePure el m a -> Iteratee el m a
liftI k = Iteratee $ return . k

ie_cont :: Monad m => Iteratee el m a -> m (IterV el m a)
ie_cont k = return $ IE_cont k Nothing
-}


-- ------------------------------------------------------------------------
-- Primitive iteratees

-- Read a stream to the end and return all of its elements as a list
-- This primitive iteratee is quite useful when writing test cases.
stream2list :: Monad m => Iteratee el m [el]
stream2list = ie_cont $ step []
 where
 step acc (Chunk []) = ie_contM (step acc)
 step acc (Chunk ls) = ie_contM (step $ acc ++ ls)
 step acc stream     = ie_doneM acc stream

-- Check if the stream is finished or harbors an error
is_stream_finished :: Monad m => Iteratee el m (Maybe ErrMsg)
is_stream_finished = ie_cont check
 where check (Chunk []) = ie_contM check
       check s@(EOF e)  = ie_doneM (Just $ maybe exc_EOF id e) s
       check s          = ie_doneM Nothing s

-- Offer a snapshot of the stream
snapshot_stream :: Monad m => Iteratee el m (Stream el)
snapshot_stream = ie_cont (\s -> ie_doneM s s)

-- The equivalent of Control.Exception.handle
en_handle :: Monad m => (ErrMsg -> b) -> Iteratee el m a ->
	     Iteratee el m (Either b a)
en_handle h (IE_done x) = IE_done (Right x)
en_handle h (IE_cont (Just e) _) = IE_done (Left (h e))
en_handle h (IE_cont Nothing k)  = IE_cont Nothing (\s -> k s >>= check)
  where check (i,s) = return (en_handle h i,s)

-- ------------------------------------------------------------------------
-- Primitive iteratees: parser combinators

-- The analogue of List.break
-- It takes an el predicate and returns a string of els,
-- which is the (possibly empty) prefix of the stream. None of the
-- characters in the string satisfy the el predicate.
-- If the stream is not terminated, the first el of the remaining
-- stream satisfies the predicate

break :: Monad m => (el -> Bool) -> Iteratee el m [el]
break cpred = ie_cont $ step []
 where
 step before (Chunk [])  = ie_contM (step before)
 step before (Chunk str) = case Prelude.break cpred str of
       (_,[])     -> ie_contM (step (before ++ str))
       (str,tail) -> ie_doneM (before ++ str) (Chunk tail)
 step before stream = ie_doneM before stream



-- A particular optimized case of the above: skip all elements of the stream
-- satisfying the given predicate -- until the first element
-- that does not satisfy the predicate, or the end of the stream.
-- That element (or Nothing, in case of the end of the stream)
-- is returned. The element remains on the stream.
-- This is the analogue of List.dropWhile
dropWhile :: Monad m => (el -> Bool) -> Iteratee el m (Maybe el)
dropWhile cpred = ie_cont step
 where
 step (Chunk []) = ie_contM step
 step (Chunk str) =
     case Prelude.dropWhile cpred str of
       []        ->  ie_contM step
       str@(c:_) ->  ie_doneM (Just c) (Chunk str)
 step stream = ie_doneM Nothing stream


-- Look ahead at the next element of the stream, without removing
-- it from the stream.
-- Return (Just c) if successful, return Nothing if the stream is
-- terminated (by EOF or an error)
peek :: Monad m => Iteratee el m (Maybe el)
peek = ie_cont step
 where
 step (Chunk [])      = ie_contM step
 step s@(Chunk (c:_)) = ie_doneM (Just c) s
 step stream          = ie_doneM Nothing stream


-- Attempt to read the next element of the stream and return it.
-- Raise a (recoverable) error if the stream is terminated
head :: Monad m => Iteratee el m el
head = ie_cont step
 where
 step (Chunk [])     = ie_contM step
 step (Chunk (c:t))  = ie_doneM c (Chunk t)
 step stream         = return (IE_cont (Just (setEOF stream)) step, stream)

-- Attempt to read the next element of the stream:
-- The same as head, but returning the Maybe result rather than
-- raising an error.
headM :: Monad m => Iteratee el m (Maybe el)
headM = ie_cont step
 where
 step (Chunk [])       = ie_contM step
 step (Chunk (c:t))    = ie_doneM (Just c) (Chunk t)
 step s@(EOF Nothing)  = ie_doneM Nothing s
 step s@(EOF (Just e)) = return (throwErr e, s)

-- Given a sequence of elements, attempt to match them against
-- the elements on the stream. Return the count of how many
-- elements matched. The matched elements are removed from the
-- stream.
-- For example, if the stream contains "abd", then (heads "abc") 
-- will remove the characters "ab" and return 2.
heads :: (Eq el, Monad m) => [el] -> Iteratee el m Int
heads str = loop 0 str
 where
 loop cnt []        = return cnt
 loop cnt str       = ie_cont (step cnt str)
 step cnt str (Chunk [])          = ie_contM (step cnt str)
 step cnt (c:t) s@(Chunk (c':t')) = 
     if c == c' then step (succ cnt) t (Chunk t') 
	else ie_doneM cnt s
 step cnt _ stream                = ie_doneM cnt stream

-- Skip the rest of the stream
skip_till_eof :: Monad m => Iteratee el m ()
skip_till_eof = ie_cont step
 where
 step (Chunk _) = ie_contM step
 step s         = ie_doneM () s

-- Skip n elements of the stream, if there are that many
-- This is the analogue of List.drop
drop :: Monad m => Int -> Iteratee el m ()
drop 0 = return ()
drop n = ie_cont $ step n
 where
 step n (Chunk str) | length str < n = ie_contM (step (n - length str))
 step n (Chunk str) = ie_doneM () (Chunk (Prelude.drop n str))
 step _ stream      = ie_doneM () stream

-- Return the current chunk: as much as of the stream as it is 
-- _immediately_ available.
-- If nothing is immediately available, the function suspends.
-- It returns [] on EOF or an error
chunk :: Monad m => Iteratee el m [el]
chunk = ie_cont step
 where
 step (Chunk []) = ie_contM step
 step (Chunk xs) = ie_doneM xs (Chunk [])
 step stream     = ie_doneM [] stream


-- ------------------------------------------------------------------------
-- Combining the primitive iteratees to solve the running problem:
-- Reading headers and the content from an HTTP-like stream

type Line = String	-- The line of text, terminators are not included

-- Read the line of text from the stream
-- The line can be terminated by CR, LF or CRLF.
-- Return (Right Line) if successful. Return (Left Line) if EOF or
-- a stream error were encountered before the terminator is seen.
-- The returned line is the string read so far.
-- This is a totally high-level Iteratee, built by composing low-level
-- ones. It knows nothing about the representation of Iteratees.

-- Compare the code below with GHCBufferIO.line_lazy

line :: Monad m => Iteratee Char m (Either Line Line)
line = break (\c -> c == '\r' || c == '\n') >>= 
       \l -> terminators >>= check l
 where
 check l 0  = return $ Left  l			-- no terminator was found
 check l _  = return $ Right l
 terminators =  heads "\r\n" >>= \n -> if n == 0 then heads "\n" else return n

-- Line iteratees: processors of a stream whose elements are made of Lines

-- Collect all read lines and return them as a list
-- see stream2list

-- Print lines as they are received. This is the first `impure' iteratee
-- with non-trivial actions during chunk processing
print_lines :: Iteratee Line IO ()
print_lines = ie_cont step
 where
 step (Chunk []) = ie_contM step
 step (Chunk ls) = mapM_ pr_line ls >> ie_contM step
 step s@(EOF Nothing) = putStrLn ">> natural end"   >> ie_doneM () s
 step s@(EOF e)       = putStrLn (">> unnatural end: "  ++ show e) >> 
			 ie_doneM () s
 pr_line line = putStrLn $ ">> read line: " ++ line

-- ------------------------------------------------------------------------
-- Enumerators
-- Each enumerator takes an iteratee and returns an iteratee:
-- an Enumerator is an iteratee transformer.
-- The enumerator normally stops when the stream is terminated
-- or when the iteratee moves to the done state, whichever comes first.
-- When to stop is of course up to the enumerator...

-- We have two choices of composition: compose iteratees or compose
-- enumerators. The latter is useful when one iteratee
-- reads from the concatenation of two data sources.

-- We can define Enumerator to be literally an Iteratee transformer:
-- type Enumerator el m a = Iteratee el m a -> Iteratee el m a

{-
All enumerators, even enum_file below, can be implemented to have the
above signature. We can use regular functional composition to compose
Enumerators. The approach turns out successful -- but flawed.
  
Still, there is a dissatisfaction. Consider the following code
  
        do
        let iter = enum_file file1 iter_count
        some_action
        run $ enum_file file2 iter
  
where iter_count is, for example, an iteratee that returns the count
of items in the input stream. The code returns the combined count of
characters in file1 and file2. It indeed does that. The question is:
when exactly some_action is performed relative to the opening and
closing of file1? That is, is some_action done before file1 is
opened? A more important question: is file1 opened before file2?

The answer to the first question is clear: some_action is done before
file1 is opened. The result of (enum_file file1 iter_count) is a pure
value Iteratee m a. That value encapsulates an action but the action
is not performed yet. The question about the order of opening of file1
and file2 cannot be answered from the above code. The value iter
encapsulates the action of opening and closing file1. If the
enumerator enum_file executes the action in iter as the very first thing,
then file1 will be opened before file2. But nothing forces enum_file
to behave this way; it may open file2 before checking the result of
iter. Then file2 would be opened before file1.

So, although my experiment works, we lose the precise control on
action sequencing. If we used pure computations, that is no
loss. There results are the same in either way. When we deal with IO,
we have to be very precise with the sequence of actions. When
enumerator has the type
        Iteratee m a -> m (Iteratee m a)
then there is no longer uncertainty about the order of opening the
files. Since enumerator takes Iteratee but produces the monadic value
m (Iteratee m a), we have to run the action to get Iteratee m a, in order to
pass to the next enumerator. The type system itself forces the
sequencing on us. That property is well worth preserving, even at the
expense of burdening the notation with extra combinators such as
>>>.
-}

type Enumerator el m a = Iteratee el m a -> m (Iteratee el m a)

-- GUIDELINES for writing enumerators
-- 
-- It is typical for an enumerator to disregard the remaining-stream
-- part of the state of the Iteratee computations. Some enumerators
-- may use this remaining stream data to report the location of an error
-- in stream terms, for example.
-- That scenario is incorporated in the function feedI. Enumerator
-- should use it to feed the data to the iteratee.
--
-- The enumerator should only handle the iteratee that is in
-- the ``more data'' state, that is, (IE_cont Nothing k).
-- In all other cases, the enumerator should return the 
-- iteratee argument as it is (finishing off the enumeration
-- process if started). Therefore, if Iteratee reports an error,
-- it will propagate up, to enumerator that can handle the
-- error or control message, or to run.
-- Therefore, typically enumerator code has the structure
--   enum (IE_cont Nothing k) = ...
--   enum i = return i
-- Only the enumerators that wish to handle some control
-- messages themselves (e.g., seek message, see RandomIO.hs)
-- deviate from this pattern.

-- The most primitive enumerator: applies the iteratee to the terminated
-- stream. The result is the iteratee usually in the done state.
-- A `good' iteratee must move to the done state or error state
-- upon receiving the EOF.
enum_eof :: Monad m => Enumerator el m a
enum_eof (IE_cont Nothing k) = feedI k (EOF Nothing) >>= return . check
 where
 check i@IE_done{}          = i			-- done state
 check (IE_cont Nothing _)  = throwErr exc_divergent
 check (IE_cont (Just e) _) = throwErr e	-- error state
enum_eof i  = return i

-- Another primitive enumerator: tell the Iteratee the stream terminated
-- with an error
enum_err :: Monad m => ErrMsg ->  Enumerator el m a
enum_err e (IE_cont Nothing k) = feedI k (EOF (Just e)) >>= return . check
 where
 check i@IE_done{}          = i			-- done state
 check (IE_cont Nothing _)  = throwErr exc_divergent
 check (IE_cont (Just e) _) = throwErr e	-- error state
enum_err _ i  = return i


-- The composition of two enumerators: just the functional composition.
-- It is convenient to flip the order of the arguments of the composition
-- though: in e1 >>> e2, e1 is executed first.
-- A similar left-to-right composition operator is defined in
-- Control.Category.
-- The composition of enumerators is not exactly (.): we take care
-- to force the result of the enumerator e1 before passing it to e2.
-- We are thus certain that all effects of enumerating e1 happen before
-- the effects of e2.
(>>>):: Monad m => Enumerator el m a -> Enumerator el m a -> Enumerator el m a
-- (>>>) = flip (.)
(>>>) e1 e2 = \i -> e2 =<< (e1 i)


-- The pure 1-chunk enumerator
-- It passes a given string to the iteratee in one chunk
-- This enumerator does no IO and is useful for testing of base parsing
enum_pure_1chunk :: Monad m => [el] -> Enumerator el m a
enum_pure_1chunk str (IE_cont Nothing k) = feedI k (Chunk str) 
enum_pure_1chunk _   iv                  = return iv


-- The pure n-chunk enumerator
-- It passes a given string to the iteratee in chunks no larger than n.
-- This enumerator does no IO and is useful for testing of base parsing
-- and handling of chunk boundaries
enum_pure_nchunk :: Monad m => [el] -> Int -> Enumerator el m a
enum_pure_nchunk str@(_:_) n (IE_cont Nothing k) = 
    feedI k (Chunk s1) >>= enum_pure_nchunk s2 n
 where (s1,s2) = splitAt n str
enum_pure_nchunk _ _ iv = return iv
       

-- The enumerator of a POSIX Fd
-- Unlike fdRead (which allocates a new buffer on
-- each invocation), we use the same buffer all throughout enumeration
enum_fd :: Fd -> Enumerator Char IO a
enum_fd fd (IE_cont Nothing k) = 
    allocaBytes (fromIntegral buffer_size) (loop k)
 where
  buffer_size = 4096
  -- buffer_size = 5 -- for tests; in real life, there should be 1024 or so
  loop k p = do
   n <- myfdRead fd p buffer_size
   -- putStrLn $ "Read buffer, size " ++ either (const "IO err") show n
   case n of
    Left errno -> feedI k (EOF (Just (exc_IOErr errno)))
    Right 0 -> return $ ie_cont k
    Right n -> do
	 str <- peekCAStringLen (p,fromIntegral n)
	 feedI k (Chunk str) >>= check p
  check p (IE_cont Nothing k) = loop k p
  check _ i                   = return i
enum_fd _ i = return i			-- i doesn't want any input

enum_file :: FilePath -> Enumerator Char IO a
enum_file filepath iterv =  do
  putStrLn $ "opened file " ++ filepath
  fd <- openFd filepath ReadOnly Nothing defaultFileFlags
  r  <- enum_fd fd iterv
  closeFd fd
  putStrLn $ "closed file " ++ filepath
  return r

-- A generic version of enum_file that works over the arbitrary
-- MonadIO
enum_file_gen :: MonadIO m => FilePath -> Enumerator Char m a
enum_file_gen filepath (IE_cont Nothing k) = do
    liftIO $ putStrLn $ "opened file " ++ filepath
    fd <- liftIO $ openFd filepath ReadOnly Nothing defaultFileFlags
    p  <- liftIO $ mallocBytes (fromIntegral buffer_size)
    r  <- loop fd k p
    liftIO $ free p
    liftIO $ closeFd fd
    liftIO $ putStrLn $ "closed file " ++ filepath
    return r
 where
  --  buffer_size = 4096
  buffer_size = 5 -- for tests; in real life, there should be 1024 or so
  loop fd k p = do
   n <- liftIO $ myfdRead fd p buffer_size
   liftIO $ putStrLn $ "Read buffer, size " ++ either (const "IO err") show n
   case n of
    Left errno -> feedI k (EOF (Just (exc_IOErr errno)))
    Right 0 -> return $ ie_cont k
    Right n -> do
	 str <- liftIO $ peekCAStringLen (p,fromIntegral n)
	 feedI k (Chunk str) >>= check fd p
  check fd p (IE_cont Nothing k) = loop fd k p
  check _ _ i                    = return i
enum_file_gen _ i = return i			-- i doesn't want any input

-- ------------------------------------------------------------------------
-- Stream adapters: Iteratees and Enumerators at the same time
--
-- Stream adapters, or Enumeratees, handle nested streams. Stream nesting, 
-- or encapsulation, is rather common: buffering, character encoding, 
-- compression, encryption, SSL are all examples of stream nesting. On one
-- hand, an Enumeratee is an Enumerator of a nested stream:
-- it takes an iteratee for a nested stream, feeds its some data,
-- returning the resulting iteratee when the nested stream is finished
-- or when the iteratee is done. On the other hand, an Enumeratee
-- is the Iteratee for the outer stream, taking data from the parent
-- enumerator.
-- One can view an Enumeratee as a AC/DC or voltage converter, or as
-- a `vertical' composition of iteratees (compared to monadic bind, which
-- plumbs two iteratees `horizontally')
--
-- In the trivial case (e.g., Word8 to Char conversion), one element
-- of the output stream is mapped to one element of the nested stream.
-- Generally, we may need to read several elements from the outer stream to
-- produce one element for the nested stream. Sometimes we can produce
-- several nested stream elements from a single outer stream element.
--
-- That many-to-many correspondence between the outer and the nested streams
-- brings a complication. Suppose an enumeratee received an outer
-- stream chunk of two elements elo1 and elo2. The enumeratee picked 
-- elo1 and decoded it into a chunk of three elements eli1, eli2, and
-- eli3, passing the chunk to the nested iteratee. The latter has read 
-- eli1 and declared itself Done. The enumeratee has to return a value
-- that contains the result of the nested Iteratee, and the 
-- fact the element elo2 of the outer stream is left unprocessed.
-- (We stress that we do _not_ report that there  are two elements left on
-- the nested stream (eli2 and eli3): the nested stream is an internal
-- resource of an enumeratee, which we do not leak out.)  We can
-- report all these pieces of data if we pack them in a value
-- of the following type

type Enumeratee elo eli m a = 
    Iteratee eli m a -> Iteratee elo m (Iteratee eli m a)

-- We come to the same type in a different way. Suppose that the
-- enumeratee has received EOF on its stream (that is, the outer stream).
-- The enumeratee, as the outer iteratee, must move to the Done state. 
-- Yet the nested iteratee is not finished. The enumeratee then has to
-- return the nested iteratee as its result.
-- The type of Enumeratee makes it clear that all effects of the inner
-- Iteratee are absorbed into the outer Iteratee.

-- The type of Enumeratee is the type of Enumerator operating over
-- the monad that is Iteratee.

-- GUIDELINES for writing enumeratees
-- Low-level enumeratees (e.g., take) look like Iteratees
-- Higher-level enumeratees (e.g., enum_lines) use lift . feedI
-- to feed data to the inner iteratee, perform its effects
-- and wait for more data in the output stream.
-- 
-- Most of the user-written enumeratees would be high-level, like
-- enum_words below (and _unlike_ take). These high-level enumeratees
-- would not need to know the representation of iteratees.
--
-- Enumeratee is Enumerator whose base monad type is Iteratee.
-- Thus guidelines for writing Enumerators apply.



-- One of the simplest Enumeratee: the nested stream is the prefix
-- of the outer stream of exactly n elements long. Such nesting arises
-- when several independent streams are concatenated.
--
-- Read n elements from a stream and apply the given (nested) iteratee to the
-- stream of the read elements. Unless the stream is terminated early, we
-- read exactly n elements (even if the iteratee has accepted fewer).
-- The last phrase implies that
--        take n iter1 >> take m iter2
--     is different from
--        take (n+m) (iter1 >> iter2)
--  in the case iter1 receives a chunk, moves to a done state after
--  consuming a part of it. Then in (iter1 >> iter2), iter2 would get
--  the rest of the chunk. In
--        take n iter1 >> take m iter2
--  iter2 would not get the rest of iter1's chunk. In fact, 
--        take n iter1 >> take m iter2 
--  is the same as
--        drop n >> take m iter2 
-- This behavior is intended: `take' reinforces fixed-length frame boundaries.

take :: Monad m => Int -> Enumeratee el el m a
take 0 i                    = return i
take n (IE_cont Nothing k)  = ie_cont (step n k)
 where
 step n k (Chunk []) = ie_contM (step n k)
 step n k chunk@(Chunk str) | length str < n = feedI k chunk >>= \i ->
				  return (take (n - length str) i, empty_stream)
 step n k (Chunk str) = feedI k (Chunk s1) >>= \i -> ie_doneM i (Chunk s2)
  where (s1,s2) = splitAt n str
 step n k stream      = feedI k stream >>= \i -> ie_doneM i stream
take n i  = drop n >> return i


-- Map the stream: yet another Enumeratee
-- Given the stream of elements of the type elo and the function elo->eli,
-- build a nested stream of elements of the type eli and apply the
-- given iteratee to it.
-- Note the contravariance.
-- The difficult question is about left-over elements.
-- Suppose the enumeratee received a chunk of elo elements,
-- mapped them to eli elements and passed the chunk to the inner iteratee.
-- The inner iteratee moved to a done state and reported N eli elements
-- as not consumed.
-- There are two choices for the result of the Enumeratee:
--  no left-over elo elements; the inner iteratee in the Done state
--  with N left-over eli elements
--  N left-over elo elements; the inner iteratee in the Done state
--  with 0 left-over eli elements.
-- The second choice assumes that we can map from left-over eli elements
-- back to the left-over elo elements. Since we map one elo
-- element to one eli element, we can always determine how many
-- elo elements left over from the number of remaining eli elements.
-- For now, we go for the default choice: 0 left-over eli elements
-- (to enforce ``transaction boundaries'' and 0 left-over elo
-- elements.

map_stream :: Monad m => (elo -> eli) -> Enumeratee elo eli m a
map_stream f (IE_cont Nothing k) = ie_cont (step k)
 where
 step k (Chunk [])  = ie_contM (step k)
 step k (Chunk str) = feedI k (Chunk (map f str)) >>=
		      ie_ret . map_stream f
 step k s           = ie_doneM (ie_cont k) s
map_stream _ i = return i

-- Convert one stream into another, not necessarily in `lockstep'
-- The transformer map_stream maps one element of the outer stream
-- to one element of the nested stream. The transformer below is more
-- general: it may take several elements of the outer stream to produce
-- one element of the inner stream.
-- The transformation from one stream to the other is specified as
-- Iteratee elo m eli.
-- This is a generalization for Monad.sequence

sequence_stream :: Monad m => Iteratee elo m eli -> Enumeratee elo eli m a
sequence_stream fi i@(IE_cont Nothing k) = 
    is_stream_finished >>= maybe (step k) (\_ -> return i)
 where
 step k = fi >>= \v -> lift (feedI k (Chunk [v])) >>= sequence_stream fi
sequence_stream _ i = return i

-- Parallel composition of Iteratees
-- Thanks to John Lato for suggesting it.
-- The following enumeratee enum2 applies two iteratees to the same stream,
-- in effect, splitting the stream in two.
-- There is no buffering however: enum2 receives a chunk and passes
-- it to both iteratees, before asking for the next chunk.
-- The enumeratee enum2 finishes either when the stream is over or when
-- both given iteratees finish. That is, enum2 continues for as long
-- as there are stream data and at least one of the iteratees wants them.
-- The two given iteratees operate in parallel and do not communicate.
-- If the communication is desired, see Iterated Iteratees,
-- IterateeN.hs

enum2 :: Monad m => Iteratee el m a -> Iteratee el m b ->
	    Iteratee el m (Iteratee el m a, Iteratee el m b)
enum2 (IE_cont Nothing k1) (IE_cont Nothing k2) = ie_cont (step k1 k2)
  where
  step k1 k2 (Chunk [])  = ie_contM (step k1 k2)
  step k1 k2 str@Chunk{} = feedI k1 str >>= \i1 ->
			   feedI k2 str >>= \i2 ->
			   ie_ret $ enum2 i1 i2
  step k1 k2 str         = ie_doneM (ie_cont k1, ie_cont k2) str
 -- at least one iteratee is finished and doesn't want any data
enum2 i1@(IE_cont Nothing _) i2 = i1 >>= \v1 -> return (return v1, i2)
enum2 i1 i2@(IE_cont Nothing _) = i2 >>= \v2 -> return (i1, return v2)
enum2 i1 i2 = return (i1,i2)		-- both are finished

-- The following is the analogue of runI for pairs of iteratees,
-- handy to apply to the result of enum2

runI2 :: Monad m => (Iteratee el1 m a, Iteratee el2 m b) -> 
	            Iteratee el m (a,b)
runI2 (IE_done x, IE_done y) = return (x,y)
runI2 (IE_cont (Just e) _,_) = throwErr e
runI2 (_,IE_cont (Just e) _) = throwErr e
runI2 (i1,i2) = lift (run i1 >>= \v1 -> run i2 >>= \v2 ->
		            return (v1,v2))

-- A convenient combinator for the frequently occurring pattern
enumPair :: Monad m => Iteratee el m a -> Iteratee el m b ->
	    Iteratee el m (a,b)
enumPair i1 i2 = enum2 i1 i2 >>= runI2


-- ------------------------------------------------------------------------
-- Combining the primitive iteratees to solve the running problem:
-- Reading headers and the content from an HTTP-like stream

-- Convert the stream of characters to the stream of lines, and
-- apply the given iteratee to enumerate the latter.
-- The stream of lines is normally terminated by the empty line.
-- When the stream of characters is terminated, the stream of lines
-- is also terminated, abnormally.
-- This is the first proper Enumeratee: it is the iteratee of the
-- character stream and the enumerator of the line stream.
-- More generally, we could have used sequence_stream to implement enum_lines.

enum_lines :: Monad m => Enumeratee Char Line m a
enum_lines i@(IE_cont Nothing k) = line >>= check_line
 where
 check_line (Right "") = return i  -- empty line, normal exit
 check_line (Right l)  = lift (feedI k (Chunk [l])) >>= enum_lines
 check_line (Left l)  = 
     lift (feedI k (if null l then (EOF (Just exc_EOF)) else Chunk [l]) >>=
	   enum_err exc_EOF)
enum_lines i = return i


-- Convert the stream of characters to the stream of words, and
-- apply the given iteratee to enumerate the latter.
-- Words are delimited by white space.
-- This is the analogue of List.words
-- It is instructive to compare the code below with the code of
-- List.words, which is:
-- words                   :: String -> [String]
-- words s                 =  case dropWhile isSpace s of
--                                 "" -> []
--                                 s' -> w : words s''
--                                       where (w, s'') =
--                                              break isSpace s'
-- One should keep in mind that enum_words is a more general, monadic
-- function.
-- More generally, we could have used sequence_stream to implement enum_words.

enum_words :: Monad m => Enumeratee Char String m a
enum_words i@(IE_cont Nothing k) = 
    dropWhile isSpace >> break isSpace >>= check_word
 where
 check_word ""  = return i	-- finished
 check_word str = lift (feedI k (Chunk [str])) >>= enum_words 
enum_words i = return i

-- HTTP chunk decoding
-- Each chunk has the following format:
--
-- 	  <chunk-size> CRLF <chunk-data> CRLF
--
-- where <chunk-size> is the hexadecimal number; <chunk-data> is a
-- sequence of <chunk-size> bytes.
-- The last chunk (so-called EOF chunk) has the format
-- 0 CRLF CRLF (where 0 is an ASCII zero, a character with the decimal code 48).
-- For more detail, see "Chunked Transfer Coding", Sec 3.6.1 of
-- the HTTP/1.1 standard:
-- http://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.6.1

-- The following enum_chunk_decoded has the signature of the enumerator
-- of the nested (encapsulated and chunk-encoded) stream. It receives
-- an iteratee for the embedded stream and returns the iteratee for
-- the base, embedding stream. Thus what is an enumerator and what
-- is an iteratee may be a matter of perspective.

-- We have a decision to make: Suppose an iteratee has finished (either because
-- it obtained all needed data or encountered an error that makes further
-- processing meaningless). While skipping the rest of the stream/the trailer,
-- we encountered a framing error (e.g., missing CRLF after chunk data).
-- What do we do? We chose to disregard the latter problem.
-- Rationale: when the iteratee has finished, we are in the process
-- of skipping up to the EOF (draining the source).
-- Disregarding the errors seems OK then.
-- Also, the iteratee may have found an error and decided to abort further
-- processing. Flushing the remainder of the input is reasonable then.
-- One can make a different choice...

-- The code is almost identical to that in Iteratee.hs

enum_chunk_decoded :: Monad m => Enumeratee Char Char m a
enum_chunk_decoded iter = read_size
 where
 read_size = break (== '\r') >>= checkCRLF iter . check_size
 checkCRLF iter m = do 
   n <- heads "\r\n"
   if n == 2 then m else frame_err (exc "Bad Chunk: no CRLF") iter
 check_size "0" = checkCRLF iter (return iter)
 check_size str@(_:_) =
     maybe (frame_err (exc ("Bad chunk size: " ++ str)) iter) read_chunk $ 
     read_hex 0 str
 check_size _ = frame_err (exc "Error reading chunk size") iter

 read_chunk size = take size iter >>= \r ->
		   checkCRLF r $ enum_chunk_decoded r

 read_hex acc "" = Just acc
 read_hex acc (d:rest) | isHexDigit d = read_hex (16*acc + digitToInt d) rest
 read_hex acc _ = Nothing

 exc msg = toException (ErrorCall $ "Chunk decoding exc: " ++ msg)
 -- If the processing is restarted, we report the frame error to the inner
 -- Iteratee, and exit
 frame_err e iter = throwRecoverableErr (exc "Frame error")
		    (\s -> enum_err e iter >>= \i -> return (return i,s))


-- ------------------------------------------------------------------------
-- Primitive Tests
       
test_iteratee = do
   drop 1
   v1 <- head
   drop 2
   v2 <- head
   return (v1,v2)

test1 = runIdentity $ run =<< enum_pure_nchunk "abcde" 5 test_iteratee
-- ('b','e')

test2 = runIdentity $ run =<< enum_pure_nchunk "abcde" 2 test_iteratee
-- ('b','e')

-- Here we test passing `the state of parsing' from one enumerator
-- to the other
test3 = runIdentity $ run =<< enum_pure_nchunk "de" 1 =<<
	         enum_pure_nchunk "abc" 2 test_iteratee
-- ('b','e')

-- ------------------------------------------------------------------------
-- Tests


-- Pure tests, requiring no IO

test_str1 =
    "header1: v1\rheader2: v2\r\nheader3: v3\nheader4: v4\n" ++
    "header5: v5\r\nheader6: v6\r\nheader7: v7\r\n\nrest\n"

read_lines_and_one_more_line = do
     lines <- id .| enum_lines stream2list
     after <- line
     return (lines,after)

with_err iter = do
		v <- iter
		e <- is_stream_finished
		return (v,e)

testp1 =
    let (lines,rest) = runIdentity $ run =<<
	     enum_pure_1chunk test_str1 read_lines_and_one_more_line
    in
    lines == ["header1: v1","header2: v2","header3: v3","header4: v4",
	      "header5: v5","header6: v6","header7: v7"] &&
    rest == Right "rest"

testp2 =
    let (lines,rest) = runIdentity $ run =<<
	     enum_pure_nchunk test_str1 5 read_lines_and_one_more_line
    in
    lines == ["header1: v1","header2: v2","header3: v3","header4: v4",
	      "header5: v5","header6: v6","header7: v7"] &&
    rest == Right "rest"

testw1 =
    let test_str = "header1: v1\rheader2: v2\r\nheader3:\t v3"
	expected = ["header1:","v1","header2:","v2","header3:","v3"] in
    let run_test test_str =
	       runIdentity $ run =<< enum_pure_nchunk test_str 5 .|
	                               enum_words stream2list
    in
    and [run_test test_str == expected,
	 run_test (test_str ++ " ") == expected]

-- ["12"]
test_seq = print =<< run =<< enum_pure_nchunk "12345678" 5 .| 
  take 2 .| sequence_stream conv stream2list
 where
 conv = do
        snapshot_stream >>= lift . print
	c1 <- head
        snapshot_stream >>= lift . print
	c2 <- head
        snapshot_stream >>= lift . print
 	return $ [c1,c2]

-- Tests of the parallel composition: convert the stream to lines
-- and two words in parallel. The line converter stops at the empty
-- line. The word converter goes all the way.
testplw =
    let test_str = "header1: v1\rheader2: v2\r\n\r\nheader3:\t v3" in
    let run_test test_str i1 i2 =
	       runIdentity $ run =<< (enum_pure_nchunk test_str 5)
	                               (runI2 =<< (enum2 i1 i2))
        by_lines, by_words :: Iteratee Char Identity [String]
        by_lines = id .| enum_lines stream2list
        by_words = id .| enum_words stream2list
    in (run_test test_str by_lines by_lines ==
        (["header1: v1","header2: v2"],["header1: v1","header2: v2"]),
	run_test test_str by_lines by_words ==
	(["header1: v1","header2: v2"],
	 ["header1:","v1","header2:","v2","header3:","v3"]),
	run_test test_str by_words by_lines ==
	(["header1:","v1","header2:","v2","header3:","v3"],
	 ["header1: v1","header2: v2"]),
	run_test test_str by_words by_words ==
	(["header1:","v1","header2:","v2","header3:","v3"],
	 ["header1:","v1","header2:","v2","header3:","v3"]))


-- Test Fd driver

test_driver line_collector filepath = do
  fd <- openFd filepath ReadOnly Nothing defaultFileFlags
  putStrLn "About to read headers"
  result <- run =<< enum_fd fd read_lines_and_one_more_line
  closeFd fd
  putStrLn "Finished reading headers"
  case result of
   ((headers,Nothing),after) ->
       do
       putStrLn $ "The line after headers is: " ++ show after
       putStrLn "Complete headers"
       print headers
   ((headers,Just e),after) ->
       do
       putStrLn $ "Problem " ++ show e
       putStrLn "Incomplete headers"
       print headers
 where
 read_lines_and_one_more_line = do
     lines <- id .| enum_lines line_collector
     e <- is_stream_finished
     after <- line
     return ((lines,e),after)


	-- Complete headers, up to "header7: v7"
test11 = test_driver stream2list "test1.txt"
	-- The same
test12 = test_driver stream2list "test2.txt"
	-- "header3: v3", then EOF
test13 = test_driver stream2list "test3.txt"
	-- Incomplete headers [], EOF
test14 = test_driver stream2list "/dev/null"

test21 = test_driver print_lines "test1.txt"
test22 = test_driver print_lines "test2.txt"
test23 = test_driver print_lines "test3.txt"
test24 = test_driver print_lines "/dev/null"

-- Run the complete test, reading the headers and the body

-- This simple iteratee is used to process a variety of streams:
-- embedded, interleaved, etc.
line_printer = enum_lines print_lines

-- Two sample processors

-- Read the headers, print the headers, read the lines of the chunk-encoded
-- body and print each line as it has been read
read_headers_print_body = do
     headers <- with_err .| enum_lines stream2list
     case headers of
	(headers, Nothing) -> lift $ do
	   putStrLn "Complete headers"
	   print headers
	(headers, Just err) -> lift $ do
	   putStrLn $ "Incomplete headers due to " ++ show err
	   print headers
     lift $ putStrLn "\nLines of the body follow"
     id .| enum_chunk_decoded line_printer

-- Read the headers and print the header right after it has been read
-- Read the lines of the chunk-encoded body and print each line as
-- it has been read
print_headers_print_body = do
     lift $ putStrLn "\nLines of the headers follow"
     line_printer
     lift $ putStrLn "\nLines of the body follow"
     id .| enum_chunk_decoded line_printer

test_driver_full iter filepath = do
  fd <- openFd filepath ReadOnly Nothing defaultFileFlags
  putStrLn "About to read headers"
  run =<< enum_fd fd iter
  closeFd fd
  putStrLn "Finished reading"

test31 = test_driver_full read_headers_print_body "test_full1.txt"
{-
Complete headers
["header1: v1","header2: v2","header3: v3","header4: v4"]
Problem Just "EOF"
Incomplete body
["body line 1","body line    2","body line       3","body line          4"]
-}

test32 = test_driver_full read_headers_print_body "test_full2.txt"
-- *** Exception: control message: Just Chunk decoding exc: Frame error

test33 = test_driver_full read_headers_print_body "test_full3.txt"
{-
Complete headers
["header1: v1","header2: v2","header3: v3","header4: v4"]
Problem Just "EOF"
Incomplete body
["body line 1","body line    2","body line       3","body line          4","body line             5"]
-}

test34 = test_driver_full print_headers_print_body "test_full3.txt"


-- Interleaved reading from two descriptors using select
--
-- If the two arguments are the names of regular files, the driver
-- does simple round-robin interleaving, reading a block from one
-- file and a block from the other file. If the arguments name
-- pipes or devices, the reading becomes truly supply-driven.
-- We use select for multiplexing.
-- The first argument is the reader-iteratee. It is exactly
-- the same iteratee that is being used in the `sequential' tests above.
-- By design, two Fds are being read independently and in parallel,
-- closely emulating two OS processes each reading from their own file.
-- The code below is a simple, round-robin OS scheduler.
test_driver_mux iter fpath1 fpath2 = do
  fd1 <- openFd fpath1 ReadOnly Nothing defaultFileFlags
  fd2 <- openFd fpath2 ReadOnly Nothing defaultFileFlags
  let fds = [fd1,fd2]
  putStrLn $ "Opened file descriptors: " ++ show fds
  allocaBytes (fromIntegral buffer_size) $ loop (zip fds (repeat iter))
  mapM_ closeFd fds
  putStrLn $ "Closed file descriptors. All done"
 where
  -- we use one single IO buffer for reading
  buffer_size = 5 -- for tests; in real life, there should be 1024 or so
  loop fjque buf = do
    let fds = get_fds fjque
    if null fds then return ()
       else do
	    selected <- select'read'pending fds
	    case selected of
	      Left errno -> putStrLn "IO Error" >>
			    tell_iteratee_err (exc_IOErr errno) fjque >>
			    return ()
	      Right []   -> loop fjque buf
	      Right sel  -> process buf sel fjque

  -- get Fds from the jobqueue for the unfinished iteratees
  get_fds = foldr (\ (fd,iter) acc ->
		       case iter of {IE_cont Nothing _ -> fd:acc; _ -> acc}) []

  -- find the first ready jobqueue element,
  -- that is, the job queue element whose Fd is in selected.
  -- Return the element and the rest of the queue
  get_ready selected jq = (e, before ++ after)
    where (before,e:after) = Prelude.break (\(fd,_) -> fd `elem` selected) jq

  process buf selected fjque = do
    let ((fd,IE_cont Nothing step),fjrest) = get_ready selected fjque
    n <- myfdRead fd buf buffer_size
    putStrLn $ unwords ["Read buffer, size", either (const "IO err") show n,
			"from fd", show fd]
    case n of
     Left errno -> step (EOF (Just (exc_IOErr errno))) >>
		   loop fjrest buf
     Right 0    -> step (EOF Nothing) >>
		   loop fjrest buf
     Right n -> do
	 str     <- peekCAStringLen (buf,fromIntegral n)
	 (im,_)  <- step (Chunk str)
	 loop (fjrest ++ [(fd,im)]) buf -- round-robin

  tell_iteratee_err err = mapM_ (\ (_,iv) -> enum_err err iv)


-- Running these tests shows true interleaving, of reading from the
-- two file descriptors and of printing the results. All IO is interleaved,
-- and yet it is safe. No unsafe operations are used.
testm1 = test_driver_mux line_printer "test1.txt" "test3.txt"

testm2 = test_driver_mux print_headers_print_body
	 "test_full2.txt" "test_full3.txt"

--  LocalWords:  enumeratee INLINE
