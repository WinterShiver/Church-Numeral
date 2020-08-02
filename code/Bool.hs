-- Functional definition of booleans

true = const -- true = \x y -> x
false = flip const -- false = \x y -> y

-- Operations of booleans

not = flip
and b1 b2 = b1 b2 false
or b1 b2 = b1 true b2
xor b1 b2 = b1 (not b2) b2