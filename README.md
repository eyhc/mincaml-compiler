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

Pour compiler le code asml :
```sh
ocaml/mincamlc test.mml -asml -o test.asml
```

Pour compiler vers ARM :
```sh
ocaml/mincamlc test.mml -o test.s 
```

### Configuration des Optimisations

Pour fixer le nombre de fois où on applique les optimisations (défaut 500).
Par exemple :
```sh
ocaml/mincamlc test.mml -o test.s -n_iter 1000
```

Pour fixer la taille maximale des fonctions dont le code est substitué par l'inline expansion (défaut 10).
Par exemple :
```sh
ocaml/mincamlc test.mml -o test.s -n_iter 1000
```

### Ensemble de tests

Pour lancer l'ensemble des scripts de tests, utiliser simplement `make test` dans le dossier `ocaml/`.

### Tests avec qemu

Pour exécuter un fichier assembleur généré précédemment par appel à mincamlc et enregistré dans le dossier `ocaml/`, faire :

```sh
cp ocaml/*.s ARM/
cd ARM/
make test
```

### Debug

On explique ici comment utiliser l'option `-test`:

```sh
# afficher le résultat de la k-normalisation
ocaml/mincamlc -test-knorm test.mml

# afficher le résultat de la k-normalisation et de l'alpha-conversion
ocaml/mincamlc -test-alpha test.mml

# afficher le résultat après un tour d'optimisation
ocaml/mincamlc -test-optim test.mml

# afficher le résultat après :
#  - k-normalisation
#  - alpha-conversion
#  - let-reduction
ocaml/mincamlc -test-let test.mml

# afficher le résultat après :
#  - k-normalisation
#  - alpha conversion
#  - let-reduction
#  - closure conversion
ocaml/mincamlc -test-closure test.mml

# afficher le résultat après :
#  - asml conversion
#  - register allocation
ocaml/mincamlc -test-back test.mml
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