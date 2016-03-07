# purescript-stackless

Helps with writing stack-safe code in cases where it is hard to restructure the code into a tail recursive form.

**Status**: Seems to be working. Not heavly tested.

```StacklessT``` transforms the base monad so that it continiously swaps stack-space
for heap-space on every ```map```, ```apply``` and ```bind```.
So you can write code without using ```tailRecM``` and still avoid Stack Overflow.

For ```StacklessT``` the base monad must be an instance of ```MonadRec``` when unboxing it.
