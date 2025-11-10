# `foo`: Foo module - demonstrates cross-module type usage

This module extensively uses types from the bar module
to demonstrate documentation of cross-module references.



## `create_data`(value: *str*) → *bar.Data*

Create a new Data instance

    Args:
        value: Value for the data

    Returns:
        A new bar.Data instance with auto-generated label

    Examples:
        >>> d = create_data("test")
        >>> d.get_value()
        "test"
    


## `transform_data`(d: *bar.Data*, f: *(str) -> str*) → *bar.Data*

Transform data by applying function to its value

    Args:
        d: Data object to transform
        f: Transformation function

    Returns:
        New bar.Data with transformed value

    See Also:
        bar.process_data: For simple processing
        bar.combine_data: For combining multiple data objects
    


## `analyze_multiple`(data_list: *list[bar.Data]*) → *dict[str, int]*

Analyze multiple data objects

    Args:
        data_list: List of bar.Data objects

    Returns:
        Statistics about the data

    Raises:
        ValueError: If list is empty
    


## *class* `DataManager`(object)

Manages a collection of bar.Data objects

    This class provides high-level operations on collections
    of Data objects from the bar module.
    

**Attributes:**

- `storage`: *list[bar.Data]*
- `default`: *bar.Data*
- `name`: *str*

**Methods:**

- `__init__`(self, name: *str*)

  Initialize manager

        Args:
            name: Name for this manager
        
- `add_data`(self, d: *bar.Data*)

  Add a data object to storage

        Args:
            d: bar.Data object to add

        Raises:
            ValueError: If storage is full
        
- `find_by_label`(self, label: *str*) → *?bar.Data*

  Find first data with matching label

        Args:
            label: Label to search for

        Returns:
            Matching bar.Data or None if not found
        
- `get_or_default`(self, label: *str*) → *bar.Data*

  Get data by label or return default

        Args:
            label: Label to search for

        Returns:
            Found bar.Data or the default instance
        
- `transform_all`(self, f: *(bar.Data) -> bar.Data*) → *list[bar.Data]*

  Transform all stored data

        Args:
            f: Transformation function

        Returns:
            List of transformed bar.Data objects
        


## `process_with_prefix`(d: *bar.Data*, prefix: *str*) → *bar.Data*

Process data with a prefix

    Args:
        d: Data to process
        prefix: Prefix to add to data values

    Returns:
        New bar.Data with prefixed value
    


## *actor* `DataHandler`(initial: *bar.Data*)

Actor that handles bar.Data objects

    Args:
        initial: Initial bar.Data to store
    


## `apply_transformation`(d: *bar.Data*, transform: *(str) -> str*) → *bar.Data*

Apply transformation to bar.Data value

    Args:
        d: Data to transform
        transform: Function to apply to value

    Returns:
        New bar.Data with transformed value
    


## `apply_to_list`(items: *list[bar.Data]*, f: *(bar.Data) -> str*) → *list[str]*

Apply function to all items in list

    Args:
        items: List of bar.Data objects
        f: Function to apply

    Returns:
        List of results
    


## `process_nested`(data: *list[(str, bar.Data)]*) → *dict[str, list[bar.Data]]*

Process nested data structures

    Args:
        data: List of tuples containing keys and bar.Data

    Returns:
        Dictionary grouping bar.Data by key
    


## *actor* `main`(env)

Main actor demonstrating bar module usage