# steroid

A library for dynamic verification of multithreaded programs.

Benchmark of concurrent programs
https://github.com/unsw-corg/PTABen

## Marcelo's TODO list

x enviar um partial order para haskell
x manipular os partial orders em haskell
- transformar execucoes em partial orders de eventos
- ler executions 

## Cesar's TODO list

x decidir sobre si el tipo de los eventos en el stream es el bueno
x retornar a main, \_rt\_end
x conseguir sacar los LD y los ST y MALLOC, CALL, RET, ALLOCA
x implementar mecanismo "jaula", implementar "rt\_end"
x hacer printf de stream desde el host
x testear que todo esto funciona
- move the public stuff of rt/rt.h to a new header and leave in rt/rt.h only the
  internal stuff
