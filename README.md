# LCI

LCI is a lambda calculus interpreter. LCI provides a repl, and a function for evaluating lambda calculus "scripts," which contain top-level lambda expressions. LCI is eagerly evaluated.

## Usage

### REPL

```
lci
```

Allows step by step, or reduction to infinity of a given lambda expression.

```
lci run <script_name>
```

Reduces a given script to infinity, stack overflows, or panics if the reduction is not possible.

## Example

Here is an example step by step reduction of the paradoxical ombinator.

```
~/rust/src/github.com/dowlandaiello/lci> cargo run                                                                                                                                                11/18/2024 04:44:09 PM   Compiling lci v0.1.0 (/home/dowlandaiello/rust/src/github.com/dowlandaiello/lci)
    Finished `dev` profile [unoptimized + debuginfo] target(s) in 0.59s
     Running `target/debug/lci`
> (\x.x x)(\x.x x)
^D|1|n > 1
((\x.((x)(x)))(\x.((x)(x))))
^D|1|n > 1
((\x.((x)(x)))(\x.((x)(x))))
^D|1|n > 1
((\x.((x)(x)))(\x.((x)(x))))
^D|1|n > 1
((\x.((x)(x)))(\x.((x)(x))))
^D|1|n > 1
((\x.((x)(x)))(\x.((x)(x))))
^D|1|n > 
```
