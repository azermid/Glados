.. _variables:

Variables
#########

Variables are used to store data in a program. They are defined by a type and a name.
The type of a variable defines the size and the type of data that can be stored in the variable.
The name of a variable is used to access the data stored in the variable.


Create a variable
=================

The MLL handles the following types of variables:
- int: integer
- char: character
- bool: boolean
  
To create a variable, you need to specify the type of the variable and the name of the variable.
You can also assign a value to the variable which can be an integer, a character, a boolean or a variable.

Example
-------

.. code-block:: c

    type name = value;

    //Integer
    int i = 0;

    //Character
    char c = 'a';

    //English boolean
    bool b = True / False;

    //French boolean
    bool b = Vrai / Faux;

    //Portuguese boolean
    bool b = Verdadeiro / Falso;


Next
----

See the next section: :ref:`operators`