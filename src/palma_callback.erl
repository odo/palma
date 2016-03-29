-module(palma_callback).

-callback pool_name(Options :: term())  -> atom().
-callback pool_size(Options :: term())  -> integer().
-callback child_spec(Options :: term()) -> supervisor:child_spec().
