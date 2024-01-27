# Projet - Compilateur MinCaml

## Préambule

Le présent projet est une variante (ou une suite) du projet de mi-semestre mené en 1re année de master informatique à l'Université de Grenoble.
Il s'inspire des travaux d'Eijiro SUMII (cf http://esumii.github.io/min-caml/index-e.html)


En particulier, mes plus grands remerciements à mes camarades de projet d'alors (par ordre alphabétique): Ali BAYDOUN ; Romain BOSSY ; Jorane GUIDETTI ; Seryozha HAKOBYAN ; Noémie PELLEGRIN.

## Langage analysé par le compilateur

Il s'agit d'un sous-ensemble d'expressions d’OCaml :

```
e ::=  () | bool | int | float | x
  | - e | e1 + e2 | e1 - e2 | e1 * e2 | e1 / e2         (* entier *)
  | -.e | e1  +. e2 | e1 -. e2 | e 1 *. e2 | e1 /. e2   (* flottant *)
  | not e | e1 = e2 | e1 <= e2 | e1 < e2 | e1 >= e2 | e1 > e2 
  | if e1 then e2 else e3
  | let x = e1 in e2
  | let f x1 ... xn = e1 in e2 | e e1 ... en     (* appel d'une fonction *)
  | let (x1,...,xn) = e1 in e2 | (e1,...,en)     (* tuples *)
  | Array.create e1 e2 | e1.(e2) | e1.(e2) <- e3 (* tableaux *)
```

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

Pour fixer le nombre de fois où on applique les optimisations (défaut 50).
Par exemple pour les désactiver :
```sh
ocaml/mincamlc test.mml -o test.s -n_iter 0
```

Pour fixer la taille maximale des fonctions dont le code est substitué par l'inline expansion (défaut 10).
Par exemple :
```sh
ocaml/mincamlc test.mml -o test.s -inline_depth 50
```

### Ensemble de tests

Pour lancer l'ensemble des scripts de tests, utiliser simplement `make test` dans le dossier `ocaml/`.
Sinon pour lancer un script de test en particulier, écrivez :
```sh
./scripts/script.sh
```

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
