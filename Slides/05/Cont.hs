type Cont r a = (a -> r) -> r
-- Związek z logiką:  Cont a ∼ (a → ⊥) → ⊥ = ¬¬a

contra :: (a->b) -> (b->r) -> (a->r)
contra f g = g . f

cmap :: (a -> b) -> Cont r a -> Cont r b
--   :: (a -> b) -> ((a -> r) -> r) ->  (b -> r) -> r
cmap f m = \c -> m $ c . f -- \c -> m (contra f c)

cpure :: a -> Cont r a
cpure = flip ($) -- \a c -> c a

cbind :: Cont r a -> (a -> Cont r b) -> Cont r b
-- !!a -> (a -> !!b) -> !!b
-- ((a->r)->r)) -> (a -> (b->r)->r)
cbind m k = \c -> m (\a -> k a c)