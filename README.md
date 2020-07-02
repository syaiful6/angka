# Angka
This library provides two numeric type: Decimal and Dddouble.

### Decimal

Operation on arbitrary precision decimal number. This module provides
exact decimal arithmetic and are well suited for financial calculations.

### DDouble

Operations on double double 128-bit floating point numbers.

This package implements `double double` 128-bit floating point numbers
as a pair of IEEE `double` values. This extends the precision to 31 decimal digits
(versus 15 for `double`), but keeps the range as a double.

### Why ?

If you add two regular 64-bit `:double` values you can quickly notice the imprecision
due to decimal fractions that cannot be represented precisely. For example:

```
0.1 +. 0.2 ;;
- : float = 0.300000000000000044
```
