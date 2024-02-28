# SASL-Compiler

## Build the compiler ##

<pre><code>stack build</code></pre>

### Use its Command-line interface (CLI) ###

*   <pre><code>stack build --exec "SASL-Compiler-exe [args]"</code></pre>

*   Use the argument `-h` or `--help` for help.

### The compiler can use two different compilation strategies ###

1. The first one follows the manual.

2. The second omits the Y, and the U-combinator. It treats all definitions equally.
   This generally saves a lot of overhead, in programs that contain many local definitions, or lots of local recursion.
   Its performance improvements can be seen in the section about [benchmarking](#benchmarking).


### Start the REPL ###

*   The REPL can be started by using the `-i` (or `--repl`) flag.
     <pre><code>stack build --exec "SASL-Compiler-exe -i"</code></pre>
*    Inside the repl there's a help function which provides information about the commands of the REPL.
It can be called with `:h`.

### Functions in the standard library (Prelude) ###

*    The REPL provides a command (`:list`) to list all standard functions which are available in the REPL by default.

*    All the functions in the Prelude:
     * id
     * until
     * comp
     * map
     * fold
     * append
     * reverse
     * filter
     * sort
     * drop
     * take
     * at
     * length
     * null
     * init
     * iterate
     * repeat
     * cycle
     * splitAt
     * takeWhile
     * sum
     * product
     * plus
     * mul
     * mod
     * even
     * zipWith
     * div
     * div2
     * minus
     * minus2
     * lt
     * leq
     * eq
     * neq
     * geq
     * gt

*    Further more, the `:list` command lists all currently definied functions in the REPL.

## Special language features ##
*   Lambda abstractions:
        <pre><code>\a b . a + b</code></pre>
*   Binding a lambda:
        <pre><code>def f = \\ a b . a + b</code></pre>
*   The `$` operator (weak, right associative function application). The following evaluates to `7`:
        <pre><code>(\\x.x + 1) $ (\\x.2 * x) 3</code></pre>
*   The `'` operator (function composition). The following evaluates to `7`:
        <pre><code>(\\x . x + 1) ' (\\ x . 2 * x) $ 3 </code></pre>
*   A single-line comment:
        <pre><code>// comment</code></pre>
*   A multiline comment:
        <pre><code>/* comment...
        comment...
        comment... */</code></pre>

## Unit tests ##

*   Unit tests:
    <pre><code>stack test</code></pre>

*   Unit tests with coverage rate and html-report:
    <pre><code>stack test --coverage</code></pre>

## Documentation ##
This command generates an HTML page containing the documentation.
The path can be found in the console.
<pre><code>stack haddock</code></pre>

## Benchmarking ##
### Run the benchmarking ###
This command yields data in the console.
But it generates also a report in HTML format.
The path can be found in the console as well.

<pre><code>stack bench</code></pre>

### Some results of benchmarking ###
The results of the benchmarking depend of course on the 
machine which runs the programs.

#### Sorting ####

* Program:  
<pre><code>def testList = [7465, 95339, 79651, -68109, -73630, -40957, 7465, -18042, 27844, -52486]
def append3 l1 l2 = append $ append l1 l2
def qsort list = if list = nil then nil
                               else append3 (qsort l) [p] (qsort r)
                                where
                                   p  = hd list;
                                   rl = tl list;
                                   l  = filter (geq p) rl;
                                   r  = filter (lt p) rl
.
qsort testList</code></pre>

* Results:
<pre><code>benchmarking qsort/compiled without optimizer and optimizedCompiler
time                 895.4 ms   (889.4 ms .. 905.2 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 893.1 ms   (892.1 ms .. 894.6 ms)
std dev              1.451 ms   (335.2 μs .. 1.930 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking qsort/compiled with optimizer
time                 884.4 ms   (874.8 ms .. 889.3 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 885.8 ms   (884.5 ms .. 886.9 ms)
std dev              1.352 ms   (562.4 μs .. 1.809 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking qsort/compiled with optimizedCompiler
time                 405.1 ms   (398.8 ms .. 412.1 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 405.8 ms   (403.9 ms .. 407.2 ms)
std dev              1.955 ms   (912.6 μs .. 2.695 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking qsort/compiled with optimizer and optimizedCompiler
time                 402.4 ms   (392.3 ms .. 407.6 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 405.9 ms   (403.2 ms .. 406.9 ms)
std dev              1.823 ms   (194.7 μs .. 2.282 ms)
variance introduced by outliers: 19% (moderately inflated)</code></pre>

#### Fibonacci numbers ####

* Program:
<pre><code>def fibs = 0 : 1 : zipWith (\x y.x + y) fibs (tl fibs)
.
take 1000 fibs</code></pre>

* Results:
<pre><code>benchmarking fibs/compiled without optimizer and optimizedCompiler
time                 851.2 ms   (837.6 ms .. 876.8 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 843.7 ms   (838.8 ms .. 848.6 ms)
std dev              6.071 ms   (3.580 ms .. 7.401 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking fibs/compiled with optimizer
time                 841.7 ms   (837.4 ms .. 847.0 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 841.2 ms   (840.5 ms .. 841.7 ms)
std dev              742.5 μs   (242.8 μs .. 977.2 μs)
variance introduced by outliers: 19% (moderately inflated)

benchmarking fibs/compiled with optimizedCompiler
time                 318.7 ms   (317.1 ms .. 320.5 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 318.7 ms   (318.2 ms .. 319.1 ms)
std dev              520.7 μs   (337.4 μs .. 664.1 μs)
variance introduced by outliers: 16% (moderately inflated)

benchmarking fibs/compiled with optimizer and optimizedCompiler
time                 319.7 ms   (318.8 ms .. 321.0 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 319.3 ms   (318.7 ms .. 319.5 ms)
std dev              444.0 μs   (152.9 μs .. 638.3 μs)
variance introduced by outliers: 16% (moderately inflated)</code></pre>
