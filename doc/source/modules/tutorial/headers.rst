.. _headers:

Headers
#######

Headers allows you to include functions that are located in different files. To include a header, you need to specify the name of the header file that you want to include.

Example
-------

In `myFunction.mll` :

.. code-block:: c

    int myFunction() {
        return 42;
    }


In `main.mll` :

.. code-block:: c

    #include "myFunction.mll"

    int main() {
        int result = myFunction();
        // result = 42
    
        return 0;
    }
