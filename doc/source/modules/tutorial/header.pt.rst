.. _functions:

Funções
#########

O MLL permite definir funções. Uma função é um bloco de código que pode ser chamado de qualquer lugar no programa. Uma função pode receber parâmetros e retornar um valor.

Criar uma função
================

Para criar uma função, você precisa especificar o tipo de retorno da função, o nome da função e os parâmetros da função. Você também precisa especificar o bloco de código que a função executará.

Exemplo
-------

.. code-block:: c

    tipo nome(parâmetros) {
        // código a ser executado
    }

    // Exemplo
    int somar(int a, int b) {
        return a + b;
    }


Chamar uma função
================

Para chamar uma função, você precisa especificar o nome da função e os parâmetros da função.

Exemplo
-------

.. code-block:: c

    int resultado = somar(2, 3);

    // resultado = 5


Recursão
========

O MLL permite definir funções recursivas. Uma função recursiva é uma função que chama a si mesma.

Exemplo

.. code-block:: c

    int fatorial(int n) {
        if (n == 0) {
            return 1;
        } else {
            return n * fatorial(n - 1);
        }
    }

    int resultado = fatorial(5);

    // resultado = 120


Próximo
-------

Veja a próxima seção: :ref:`headers`.
