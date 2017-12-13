# passrhyme

Passrhyme is a rhyming password generator.  To build it in Mac OS X (with homebrew), run:
```
brew install openssl
stack build --extra-include-dirs=/usr/local/opt/openssl/include --extra-lib-dirs=/usr/local/opt/openssl/lib
```
(Those flags are needed the first time you build it because we depend on HsOpenSSL as one of our sources of secure random bits, and those are the locations where homebrew puts the openssl library.)

Then you an run it like this:

```
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
