// Class id's //////////////////////////////////////////////////////////////////////////////////////////////

// Basic unit of serialization ////////////////////////////////////////////////////////////////////////////

/*
  The serialization of an Acton object/value is a linked list of $ROW:s.
*/

typedef struct $ROW *$ROW;

struct $ROW {
  int class_id;
  int prefix_size;
  int blob_size;
  $ROW next;
  $WORD data[];
};

/*
  Each row consists of 
   - a class_id, unique for each class, indicating the class of the part of the syntax tree being serialized.
   - an int specifying the length of the prefix part of data.
   - an int specifying the length of the blob part of data.
   - the link to the next row in the list.
   - an array of words containing the prefix followed by the blob.

  The prefix is a sequence of positive integers, indicating the path from the root of the object abstract syntax tree down 
  to the component under consideration. The root of the tree has path [], the immediate instance variables have paths [0], [1],...
  If the first instance variable (with path [0]) is a list, say, the list elements have paths [0,0], [0,1], [0,2]...
  
  As a simple example, consider the following Acton code snippet:

  lst = []
  for i in range(0,6):
       if i%2 != 0:
           lst.append(lst[i//2])
       else:
           lst.append([i*i for i in range(0,i)])
 
  lst has type list[list[int]] and is [[],[],[0,1],[],[0,1,4,9],[0,1]]. This does not show that all equal sub-lists are shared, 
  i.e. there are only three separate sublists, [] with three refs, [0,1] (two refs) and [0,1,4,9] (one ref).

  The serialization of lst is as follows (the 'next' link is omitted). Also, for brevity, we assume that the prefix to 
  the root of the list is [], so we get only the "internal paths" within the list.

   6 0 1 6         | the first 6 is class_id for type list; the second 6 is #elements in this particular list. Note that prefix is empty.
   6 1 1 0 0       | 1st element is a list(6); prefix is [0]; #elements is 0.
  -6 1 1 1 0       | 2nd element is a shared list(-6); its prefix is [1] and it is the same as list with prefix [0]
   6 1 1 2 2       | 3rd element is a list; its prefix is [2] and #elements is 2.
   1 2 1 2 0 0     |    1st element of the sublist is an int; its prefix is [2,0] and its value is 0
   1 2 1 2 1 1     |    2nd element of sublist has prefix [2,1] and value 1.
  -6 1 1 3 0       | 4th element is a shared list; prefix is [3] and it is the same as list with prefix [0].
   6 1 1 4 4       | 5th element is a list; prefix is [4] and #elements is 4.
   1 2 1 4 0 0     |    1st element of sublist is an int, has prefix [4,0] and value 0
   1 2 1 4 1 1     |    2nd element of sublist is an int, has prefix [4,1] and value 1
   1 2 1 4 2 4     |    3rd element of sublist is an int, has prefix [4,2] and value 4
   1 2 1 4 3 9     |    4th element of sublist is an int, has prefix [4,3] and value 9
  -6 1 1 5 2       | 6th element is a shared list, with prefix [5] and is same as list with prefix [2].

QUESTION: The prefixes are used to handle sharing, but contribute to making serializations very big. To just handle sharing properly, they are not
necessary. Each row has an integer position in the list and that unique number is enough for sharing. Is there another reason to have these prefixes
in the serialization? If omitted and needed, the prefixes can be easily computed while parsing the serialized file. 


We will typically build the list of rows by appending new rows at the end. For efficiency, we therefore use a list with a header node:
*/


typedef struct $ROWLISTHEADER {
  $ROW fst;
  $ROW last;
} *$ROWLISTHEADER;


/*
  Class id's are used
    - as first component in a $ROW to indicate which type is being serialized.
    - in Hashable witnesses to support the __keyinfo__ method needed in serialization of dicts and sets
    - to index the serial$_methods array to choose correct instance of __deserialize__
*/

#define DUMMY_ID 0     // for dummy items in hashtables
#define INT_ID 1
#define FLOAT_ID 2
#define COMPLEX_ID 3
#define BOOL_ID 4
#define STR_ID 5
#define LIST_ID 6
#define DICT_ID 7
#define SET_ID 8
#define ITEM_ID 9     // dict items, set elems

// reading and writing from/to serialized files is done in blocks of BUF_SIZE bytes

#define BUF_SIZE 8192

// Interface to serialization /////////////////////////////////////////////////////////////////////////////

// All serializable types must have method tables as if they were subclasses of Serializable 

typedef struct serial$methods *serial$methods;

typedef struct $Serializable  *$Serializable;

struct $Serializable {
  serial$methods $class;
};

struct serial$methods {
  $None (*__serialize__)($Serializable, $Mapping$dict, $WORD, int, $dict, $ROWLISTHEADER); /* result returned in the last, accumulating param */
  $Serializable (*__deserialize__)($Mapping$dict, $ROW*, $dict);
};

// top-level functions for serialization of an object

$ROW $serialize($Serializable s, long prefix[], int prefix_size);
void $write_serialized($ROW row, char *file);
// $serialize_file just calls the above two functions
void $serialize_file($Serializable s, long prefix[], int prefix_size, char *file);

$Serializable $deserialize($ROW row, long *prefix, int *prefix_size);
$ROW $read_serialized(char *file);
// deserialize_file just calls the above two functions
$Serializable $deserialize_file(char *file,  long *prefix, int *prefix_size);

// Internal auxiliary types /////////////////////////////////////////////////////////////////////////////

typedef struct $PREFIX  *$PREFIX;

struct $PREFIX {
  int prefix_size;
  $WORD prefix[];
};

serial$methods serial$_methods[10];

// Hashable$PREFIX ////////////////////////////////////////////////////////////////////////////////////////


typedef struct $Hashable$PREFIX *$Hashable$PREFIX;

struct $Hashable$PREFIX$class {
    char *GCINFO;
    $bool (*__eq__)($Hashable$PREFIX, $PREFIX, $PREFIX);
    $bool (*__ne__)($Hashable$PREFIX, $PREFIX, $PREFIX);
    $int (*__hash__)($Hashable$PREFIX, $PREFIX);
};

struct $Hashable$PREFIX {
    struct $Hashable$PREFIX$class *$class;
};

struct $Hashable$PREFIX$class $Hashable$PREFIX$methods;
struct $Hashable$PREFIX *$Hashable$PREFIX$witness;


// $Hashable$WORD ////////////////////////////////////////////////////////////////////////////////////////

typedef struct $Hashable$WORD *$Hashable$WORD;

struct $Hashable$WORD$class {
    char *GCINFO;
    $bool (*__eq__)($Hashable$WORD, $WORD, $WORD);
    $bool (*__ne__)($Hashable$WORD, $WORD, $WORD);
    $int (*__hash__)($Hashable$WORD, $WORD);
};

struct $Hashable$WORD {
    struct $Hashable$WORD$class *$class;
};

struct $Hashable$WORD$class $Hashable$WORD$methods;
struct $Hashable$WORD *$Hashable$WORD$witness;


$None $enqueue($ROWLISTHEADER lst, $ROW elem);

$ROW $new_row(int class_id, int prefix_size, int blob_size, $WORD *prefix);

$Hashable $Hashable_instance(long class_id);

