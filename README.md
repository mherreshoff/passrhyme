# passrhyme

Passrhyme is a rhyming password generator.  To use it, run

```
stack build
stack exec passrhyme-exe
```

Note: you need to run the executable from the root directory of this repository so that passrhyme can find the CMU prounouncing
dictionary and the 10k word list.

Note also: the examples below are drawn from the best of four or five tries.

## Example (default meter)

```
$ stack exec passrhyme-exe
Enter a string of dots and dashes to select your meter.
(- means a stressed sylable and . means an unstressed sylable.)
Press Enter (blank input) for iambic tetrameter (.-.-.-.-)

careers subjective legends claim
brunette division ada aim
```

## Example (custom meter)

```
$ stack exec passrhyme-exe
Enter a string of dots and dashes to select your meter.
(- means a stressed sylable and . means an unstressed sylable.)
Press Enter (blank input) for iambic tetrameter (.-.-.-.-)
-..-..-
surely validity trains
rural committed explains
```
