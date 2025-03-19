.. _functions:

Functions
#########

The MLL allows you to define functions. A function is a block of code that can be called from anywhere in the program. A function can take parameters and return a value.

Create a function
=================

To create a function, you need to specify the return type of the function, the name of the function and the parameters of the function. You also need to specify the block of code that the function will execute.

Example
-------

.. code-block:: c

    type name(parameters) {
        // code to execute
    }

    // Example
    int add(int a, int b) {
        return a + b;
    }


Call a function
===============

To call a function, you need to specify the name of the function and the parameters of the function.

Example
-------

.. code-block:: c

    int result = add(2, 3);

    // result = 5


Recursion
=========

The MLL allows you to define recursive functions. A recursive function is a function that calls itself.

Example

.. code-block:: c

    int factorial(int n) {
        if (n == 0) {
            return 1;
        } else {
            return n * factorial(n - 1);
        }
    }

    int result = factorial(5);

    // result = 120


Next
----

See the next section: :ref:`headers`.