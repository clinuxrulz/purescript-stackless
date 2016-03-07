# purescript-stackless

Helps with writing stack-safe code in cases where it is hard to restructure the code into a tail recursive form.

**Status**: Imcomplete.

```SuspenderT``` transforms the base monad to provide a ```suspend``` action that will swap stack-space for heap-space.

```StacklessT``` transforms the base monad like ```SuspenderT``` does, but does not provide a ```suspend```.
Instead it continiously uses ```suspend``` internally and automatically on every ```map```, ```apply``` and ```bind```.
So you can write code without using ```suspend``` and still avoid Stack Overflow.

For both ```SuspenderT``` and ```StacklessT``` the base monad must be an instance of ```MonadRec``` when unboxing it.
