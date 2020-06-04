# BATcomputer

A simple computer that performs instructions on boxes

## Installation

In this example I used GHCI to run and interpretate the program 

## Usage

To load the environment
```bash
ghci
```
To load the code
```bash
:l BatComputer
```
The Computer can run several tasks:

Add the first two boxes together
```bash
execute (adder) [1,2,3]
```

Add two boxes together

```bash
execute (addXY 1 4) [1, 2, 3, 4]
```

Copy contents of box and place in another

```bash
execute (copyBox 1 4) [1, 2, 3, 4]
```

Run programs in sequence using \*->\*. For example:

```bash
execute (addXY 1 2 *->* addXY 1 4) [1, 1, 1, 1]
```

