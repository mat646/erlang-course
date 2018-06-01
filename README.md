# Erlang Course 

Completed assignments for Erlang Course

## Content

### Essentials

Implementation of basic operations and quicksort algorithm.

### RNP

Reverse Polish Notation calculator.

### Pollution

Module simulating pollution measurement system. 
System allows to create new stations, insert measurements and delete them.
Also there are plenty of query commands e.g. 
average pollution on given day or station with lowest pollution value.
Module provides set of tests created with HUnit.

### Ping-Pong

The basic usage of processes in Erlang.

### Pollution Server

Extended Pollution module.

### Pollution Gen Server

#### Usage

After building all .beam files:

```sh
$ erl -pa out/production/ErlangLab  
> pollution_gen_server:start_link().
```
