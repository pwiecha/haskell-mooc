module Ch3 where
    inOut:: (a -> a) -> (a -> a) -> a -> a
    inOut f g x = f (g (f x))