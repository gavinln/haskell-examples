{-
 - Monads as containers
 - https://wiki.haskell.org/Monads_as_containers
 -}


{-
 - A Monad is a container type with all elements of the same type.
 - fmap :: (Functor f) -> (a -> b) -> f a -> f b
 - When fmap is given a function that takes and a and returns b, it applies to
 - a container of a values and returns a container of b values.
 - Every Monad is a functor
 -
 - Return takes an element of type a and gives a container of type m a
 - return :: (Monda m) => a -> m a
 -
 - join takes a container of containers m (m a) and combines them into one
 - container.
 - join :: (Monad m) => m (m a) -> m a
 -
 - Bind takes a container of type (m a) and a function of type (a -> m b). It
 - maps the function over the container ot give m (m b) and then applies to
 - join to get a container of type (m b)
 -
 - (>>=) :: (Monad m) => m a -> (a -> m b) -> m b
 - xs >>= f = join (fmap f xs)
 
 instance Monad [] where
    --return :: a -> [a]
    return x = [x] -- make a list containing the one element given

    --(>>=) :: [a] -> (a -> [b]) -> [b]
    xs >>= f = concat (fmap f xs)
 -}

-- [10,20,30] >>= \x -> [x, x+1]
-- [10,11,20,21,30,31]

-- [10,20,30] >>= \x -> [x, x+1] >>= \y -> if y > 20 then [] else [y, y]
-- [10,10,11,11,20,20]

-- liftM is like fmap

liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f xs = xs >>= (return . f)

-- liftM (+1) [3]
-- [4]
-- fmap (+1) [3]
-- [4]

join :: (Monad m) => m (m a) -> m a
join xss = xss >>= id

-- join [[1], [2,3], []]
-- [1,2,3]

-- do { x <- [10,20,30]; [x, x + 1] }
-- [10,11,20,21,30,31]

-- do x <- [10,20,30]; y <- [x, x+1]; if y > 20 then [] else [y,y]
-- [10,10,11,11,20,20]

{-
 - Alternative LiftM
 liftM f xs = do a <- xs
                return (f a)
 -}
