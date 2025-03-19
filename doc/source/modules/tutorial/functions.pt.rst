.. _conditions:

Instruções If e Else
######################

Instrução If
=============

Com a instrução `if`, `si` ou `se` dependendo da língua que você deseja usar, você pode usar as condições para executar um bloco de código se a condição for verdadeira. A sintaxe é a seguinte:

.. code-block:: c

    // Inglês
    if (condition) {
        // código a ser executado
    }

    // Francês
    si (condition) {
        // code a exécuter
    }

    // Português
    se (condição) {
        // código a executar
    }

Exemplo
-------

.. code-block:: c

    int i = 0;

    // Inglês
    if (i < 5) {
        i++;
    }

    // Francês
    si (i < 5) {
        i++;
    }

    // Português
    se (i < 5) {
        i++;
    }

Instrução Else
=================

Além da instrução `if`, `si` ou `se`, você pode usar a instrução `else`, `sinon` ou `senao` para executar um bloco de código se a condição for falsa. A sintaxe é a seguinte:

.. code-block::

    // Inglês
    if (condition) {
        // código a ser executado
    } else {
        // código a ser executado se a condição for falsa
    }

    // Francês
    si (condition) {
        // code a exécuter
    } sinon {
        // code a exécuter se a condição est fausse
    }

    // Português
    se (condição) {
        // código a executar
    } senao {
        // código a executar se a condição for falsa
    }

Exemplo
-------

.. code-block:: c

    int i = 0;

    // Inglês
    if (i < 5) {
        i++;
    } else {
        i--;
    }

    // Francês
    si (i < 5) {
        i++;
    } sinon {
        i--;
    }

    // Português
    se (i < 5) {
        i++;
    } senao {
        i--;
    }

Próximo
-------

Veja a próxima seção: :ref:`loops`
