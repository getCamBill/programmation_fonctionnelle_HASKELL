##  Contenu du dépôt

Il contient les fichiers suivants :

-   `dragon.hs` est le canevas pour le dessin de la courbe du Dragon,
-   `Setup.hs`, `premiers-contacts.cabal`, `stack.yaml` sont des
    fichiers de configuration pour
    [Cabal](https://www.haskell.org/cabal/) et
    [Stack](https://docs.haskellstack.org/en/stable/README/).

Ces fichiers ne sont réellement utiles que si vous voulez utiliser ces
outils pour installer les bibliothèques nécessaires sur une machine
personnelle.


##  Utilisation de `stack` sur votre machine personnelle.

Cette section n’est pas un substitut à la documentation de l’outil.
Elle vous donne juste une commande qui peut aider à démarrer.

Si vous voulez installer la bibliothèque Gloss nécessaire pour
dessiner la courbe du Dragon en utilisant l’outil `stack` (que vous
devrez avoir installé, bien entendu), vous pourrez utiliser :

```console
stack --install-ghc build
```

Cette commande devrait éventuellement installer GHC, Gloss, et autres
dépendances puis compiler `dragon.hs` en un exécutable créé quelque
part dans le répertoire `.stack-work/install` (le chemin dépend de
votre configuration).
