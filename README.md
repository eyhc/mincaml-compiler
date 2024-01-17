# Projet M1 INFO - Compilateur MinCaml

## Groupe : LesPerdus


## Compilation

Utiliser simplement `make` dans le dossier `ocaml/`.

## Utilisation

### Principale

Pour utiliser uniquement le parser :
```sh
ocaml/mincamlc -i test.mml -p

# notons que le -i est optionnel
ocaml/mincamlc test.mml -p
```

Pour faire uniquement une analyse de type :
```sh
ocaml/mincamlc test.mml -t
```

Pour afficher uniquement l'asml :
```sh
ocaml/mincamlc test.mml -asml
```

Pour compiler vers ARM :
```sh
ocaml/mincamlc test.mml -o test.s
```

### Ensemble de tests

Pour lancer l'ensemble des scripts de tests, utiliser simplement `make test` dans le dossier `ocaml/`.

### Tests avec qemu

```
cp ocaml/*.s ARM/
make test
```

### Debug

On explique ici comment utiliser l'option `-test`:

```sh
# afficher le résultat de la knormalisation
ocaml/mincamlc -test -knorm test.mml
# notons que l'ordre importe peu
ocaml/mincamlc -test test.mml -knorm
ocaml/mincamlc test.mm -test -knorm

# afficher le résultat de la knorm et de l'apha-conversion
ocaml/mincamlc -test -alpha test.mml

# afficher le résultat après un tour d'optimisation
ocaml/mincamlc -test -optim test.mml

# afficher le résultat après :
#  - k-normalisation
#  - alpha conversion
#  - let reduction
ocaml/mincamlc -test -let test.mml

# afficher le résultat après :
#  - k-normalisation
#  - alpha conversion
#  - let reduction
#  - closure conversion
ocaml/mincamlc -test -closure test.mml

# afficher le résultat après :
#  - asml conversion
#  - register allocation
ocaml/mincamlc -test -back test.mml
```


Organization of the archive
```
ARM/     arm source example and compilation with libmincaml   
asml/    asml examples
doc/     all the documentation, start with index.hml
java/    MinCaml parser in Java + examples of tree traversal algo, if you do 
         the project in java  
mincaml/ MinCaml examples
ocaml/   MinCaml parser in OCaml, if you do the project in OCaml
scripts/ put your test scripts and symbolic links there, and add this 
         directory to your path
tests/   put your tests there
tools/   asml intepreter (linux binary)

We recommend that you add scripts/ and the dir that contains mincamlc to your
PATH.
```