# Extended Kaleidoscope Compiler Project

Tarek Sami
MPCS51300

### Build Instructions

Note: There is an external library added by use of a submodule...
When cloning, please run `git submodule update --recursive`

```
mkdir build
cd build 
cmake ..
./ekcc ../scratch
```

### Running Test Files
Optimization Level is a number between 1 and 13 (see src/optimizer.c for details)

#### Sieve Program
Recommended to run with 200,000 as max number to check
```
cd <project_root>
build/ekcc -l<optimization_level> testfiles/sieve.ek <number to search>
```


#### Fibonacci Sequence Program
Recommended to run with 45 as max fibonacci number
```
cd <project_root>
build/ekcc -l<optimization_level> testfiles/fibs.ek <number to search>
```


### Note
This compiler leaks memory like a mf...

I never finished properly deleting all of the objects, as it was done for a 
school project with limited time, and that wasn't something I was being graded on.

