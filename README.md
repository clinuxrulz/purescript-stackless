# purescript-stackless

Helps with writing stack-safe code in cases where it is hard to restructure the code into a tail recursive form.

```StacklessT``` transforms the base monad so that it continiously swaps stack-space
for heap-space on every ```map```, ```apply``` and ```bind```.
So you can write code without using ```tailRecM``` and still avoid Stack Overflow.

For ```StacklessT``` the base monad must be an instance of ```MonadRec``` when unboxing it.

**Example**: *(Taken from real code)*
In this example ```for_``` from ```purescript-foldable-traversable``` is made stack-safe by wrapping the computation
in a ```StacklessT```.
```
let gap = member2PosX - member1PosX
    n = ceil (gap / maxStudCentre)
if n > 1
  then do
    let stepX = gap / (toNumber n)
        startX = member1PosX + stepX
        xPositions = (\idx -> startX + stepX * (toNumber idx)) <$> (List.range 0 (n-2))
    runStacklessT $ for_ xPositions (\posX ->
      lift $ insertStud { posX: posX }
    )
    return unit
  else return unit
```
