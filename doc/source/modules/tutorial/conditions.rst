.. _conditions:

If and Else Statements
##################################


If statement
==============

With the `if`, `si` or `se` statement depending the language you want to use, you can use the conditions to execute a block of code if the condition is true. The syntax is the following :

.. code-block:: c

    // English
    if (condition) {
        // code to execute
    }

    // French
    si (condition) {
        // code a exécuter
    }

    // Portuguese
    se (condição) {
        // código a executar
    }

Example
-------

.. code-block:: c

    int i = 0;

    // English
    if (i < 5) {
        i++;
    }

    // French
    si (i < 5) {
        i++;
    }

    // Portuguese
    se (i < 5) {
        i++;
    }

Else statement
==================

In addition to the `if`, `si` or `se` statement, you can use the `else`, `sinon` or `senao` statement to execute a block of code if the condition is false. The syntax is the following :

.. code-block::

    // English
    if (condition) {
        // code to execute
    } else {
        // code to execute if the condition is false
    }

    // French
    si (condition) {
        // code a exécuter
    } sinon {
        // code a exécuter si la condition est fausse
    }

    // Portuguese
    se (condição) {
        // código a executar
    } senao {
        // código a executar se a condição for falsa
    }

Example
-------

.. code-block:: c

    int i = 0;

    // English
    if (i < 5) {
        i++;
    } else {
        i--;
    }

    // French
    si (i < 5) {
        i++;
    } sinon {
        i--;
    }

    // Portuguese
    se (i < 5) {
        i++;
    } senao {
        i--;
    }


Next
----

See the next section: :ref:`loops`