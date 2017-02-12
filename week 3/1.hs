
f x y = y:x


g x y = x:x:y

mrepeat f 0 x = x
mrepeat f n x = f (mrepeat f (n-1) x)

k 0 x = 0

