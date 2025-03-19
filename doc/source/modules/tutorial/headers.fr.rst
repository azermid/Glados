.. _functions:

Fonctions
#########

Le MLL vous permet de définir des fonctions. Une fonction est un bloc de code qui peut être appelé depuis n'importe où dans le programme. Une fonction peut prendre des paramètres et retourner une valeur.

Créer une fonction
=================

Pour créer une fonction, vous devez spécifier le type de retour de la fonction, le nom de la fonction et les paramètres de la fonction. Vous devez également spécifier le bloc de code que la fonction exécutera.

Exemple
-------

.. code-block:: c

    type nom(paramètres) {
        // code à exécuter
    }

    // Exemple
    int additionner(int a, int b) {
        return a + b;
    }


Appeler une fonction
====================

Pour appeler une fonction, vous devez spécifier le nom de la fonction et les paramètres de la fonction.

Exemple
-------

.. code-block:: c

    int resultat = additionner(2, 3);

    // resultat = 5


Récursion
=========

Le MLL vous permet de définir des fonctions récursives. Une fonction récursive est une fonction qui s'appelle elle-même.

Exemple

.. code-block:: c

    int factorielle(int n) {
        if (n == 0) {
            return 1;
        } else {
            return n * factorielle(n - 1);
        }
    }

    int resultat = factorielle(5);

    // resultat = 120


Suite
-----

Consultez la section suivante : :ref:`headers`.
