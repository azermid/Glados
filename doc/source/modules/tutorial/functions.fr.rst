.. _conditions:

Instructions If et Else
##########################

Instruction If
==============

Avec l'instruction `if`, `si` ou `se` en fonction de la langue que vous souhaitez utiliser, vous pouvez utiliser les conditions pour exécuter un bloc de code si la condition est vraie. La syntaxe est la suivante :

.. code-block:: c

    // Anglais
    if (condition) {
        // code à exécuter
    }

    // Français
    si (condition) {
        // code à exécuter
    }

    // Portugais
    se (condição) {
        // código a executar
    }

Exemple
-------

.. code-block:: c

    int i = 0;

    // Anglais
    if (i < 5) {
        i++;
    }

    // Français
    si (i < 5) {
        i++;
    }

    // Portugais
    se (i < 5) {
        i++;
    }

Instruction Else
==================

En plus de l'instruction `if`, `si` ou `se`, vous pouvez utiliser l'instruction `else`, `sinon` ou `senao` pour exécuter un bloc de code si la condition est fausse. La syntaxe est la suivante :

.. code-block::

    // Anglais
    if (condition) {
        // code à exécuter
    } else {
        // code à exécuter si la condition est fausse
    }

    // Français
    si (condition) {
        // code à exécuter
    } sinon {
        // code à exécuter si la condition est fausse
    }

    // Portugais
    se (condição) {
        // código a executar
    } senao {
        // código a executar se a condição for falsa
    }

Exemple
-------

.. code-block:: c

    int i = 0;

    // Anglais
    if (i < 5) {
        i++;
    } else {
        i--;
    }

    // Français
    si (i < 5) {
        i++;
    } sinon {
        i--;
    }

    // Portugais
    se (i < 5) {
        i++;
    } senao {
        i--;
    }

Suite
-----

Consultez la section suivante : :ref:`loops`
