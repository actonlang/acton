
class ListNp(object):

    @staticmethod
    def array(l):
        return l.copy()
    
    @staticmethod
    def shape(lst):
        def ishape(lst):
            shapes = [ishape(x) if isinstance(x, list) else [] for x in lst]
            shape = shapes[0]
            if shapes.count(shape) != len(shapes):
                raise ValueError('Ragged list')
            shape.append(len(lst))
            return shape
        return tuple(reversed(ishape(lst)))

    @staticmethod
    def fillval(shape, val):
        if shape is None or not isinstance(shape, tuple) or len(shape) <= 0:
            return None
        def rfillval(shape, val):
            if shape is None or not isinstance(shape, tuple) or len(shape) <= 0:
                return None
            if len(shape) == 1:
                return [ val for i in range(shape[0]) ]
            else:
                l = []        
                for i in range(shape[0]):
                    rec = rfillval(shape[1:], val)
                    l.append(rec.copy())
                return l
        return rfillval(shape, val)

    @staticmethod
    def zeros(shape):
        return ListNp.fillval(shape, 0)

    @staticmethod
    def ones(shape):
        return ListNp.fillval(shape, 1)
    
    @staticmethod
    def zeros_like(l):
        return ListNp.fillval(ListNp.shape(l), 0)

    @staticmethod
    def ones_like(l):
        return ListNp.fillval(ListNp.shape(l), 1)

    # Aggregations on a single array's elements:
    
    @staticmethod
    def min(l):
        return min(l)
        
    @staticmethod
    def max(l):
        return max(l)

    @staticmethod
    def argmin(l):
        return l.index(min(l))
        
    @staticmethod
    def argmax(l):
        return l.index(max(l))

    @staticmethod
    def sum(l):
        assert len(ListNp.shape(l)) == 1
        return sum(l)

    @staticmethod
    def mean(l):
        assert len(ListNp.shape(l)) == 1
        return sum(l) / len(l)

    @staticmethod
    def stdev(l, sample=True):
        # Calculates sample stedv by default. Call with sample=False to calculate population stdev
        assert len(ListNp.shape(l)) == 1
        assert len(l) >= 2 # stdev requires at least 2 data points
        c = ListNp.mean(l)
        ss = sum((x-c)**2 for x in l)
        pvar = ss/(n if sample else (n-1))
        return pvar**0.5

    @staticmethod
    def sort(l):
        assert len(ListNp.shape(l)) == 1
        return sorted(l)
    
    @staticmethod
    def median(l):
        assert len(ListNp.shape(l)) == 1
        quotient, remainder = divmod(len(lst), 2)
        if remainder:
            return sorted(lst)[quotient]
        return sum(sorted(lst)[quotient - 1:quotient + 1]) / 2.
        
    # Operations between arrays and scalars:
        
    @staticmethod
    def plus(l, n):
        # Allow list and constant arguments to be given in any order:
        if isinstance(l, list):
            return [ i + n for i in l]
        else:
            return [ i + l for i in n]

    @staticmethod
    def minus(l, n):
        # Allow list and constant arguments to be given in any order:
        if isinstance(l, list):
            return [ i - n for i in l]
        else:
            return [ i - l for i in n]

    @staticmethod
    def negate(l):
        return [ -i for i in l]
        
    @staticmethod
    def mul(l, n):
        # Allow list and constant arguments to be given in any order:
        if isinstance(l, list):
            return [ i * n for i in l]
        else:
            return [ i * l for i in n]

    @staticmethod
    def div(l, n):
        # Allow list and constant arguments to be given in any order:
        if isinstance(l, list):
            return [ i / n for i in l]
        else:
            return [ i / l for i in n]

    @staticmethod
    def power(l, n):
        # Allow list and constant arguments to be given in any order:
        if isinstance(l, list):
            return [ pow(i, n) for i in l]
        else:
            return [ pow(i, l) for i in n]
        
    @staticmethod
    def abs(l):
        return [ abs(i) for i in l]

    # Indexing operations:
    
    @staticmethod
    def lt(l, t):
        return [ i < t for i in l ]

    @staticmethod
    def lte(l, t):
        return [ i <= t for i in l ]

    @staticmethod
    def gt(l, t):
        return [ i > t for i in l ]

    @staticmethod
    def gte(l, t):
        return [ i >= t for i in l ]

    @staticmethod
    def eq(l, t):
        return [ i == t for i in l ]

    @staticmethod
    def neq(l, t):
        return [ i != t for i in l ]

    @staticmethod
    def bindex(l, index):
        return [ l[i] for i,b in enumerate(index) if b ]

    @staticmethod
    def set_bindex(l, index, l2):
        for idx, el in zip([i for i,b in enumerate(index) if b],l2):
            l[idx] = el

    @staticmethod
    def index(l, index):
        return [ l[i] for i in index ]

    @staticmethod
    def set_index(l, index, l2):
        for idx, el in zip(index,l2):
            l[idx] = el
    
    @staticmethod
    def arange(start, stop, step = None):
        if step is not None:
            return [i for i in range(start, stop, step)]
        else:
            return [i for i in range(start, stop)]
            
    # Operations between 2 arrays:
            
    @staticmethod
    def add(l1, l2):
        assert ListNp.shape(l1) == ListNp.shape(l2)
        return [ l1[i] + l2[i] for i in range(len(l1)) ]

    @staticmethod
    def subtract(l1, l2):
        assert ListNp.shape(l1) == ListNp.shape(l2)
        return [ l1[i] - l2[i] for i in range(len(l1)) ]

    @staticmethod
    def multiply(l1, l2):
        assert ListNp.shape(l1) == ListNp.shape(l2)
        return [ l1[i] * l2[i] for i in range(len(l1)) ]

    @staticmethod
    def divide(l1, l2):
        assert ListNp.shape(l1) == ListNp.shape(l2)
        return [ l1[i] / l2[i] for i in range(len(l1)) ]
    
    @staticmethod
    def bitwise_and(l1, l2):
        assert ListNp.shape(l1) == ListNp.shape(l2)
        return [ (l1[idx] & l2[idx]) for idx in range(len(l1)) ]

    @staticmethod
    def bitwise_or(l1, l2):
        assert ListNp.shape(l1) == ListNp.shape(l2)
        return [ (l1[idx] | l2[idx]) for idx in range(len(l1)) ]

    @staticmethod
    def transpose(l):
        transposed = []
        for i in l:
            transposed.append([i])
        return transposed

    @staticmethod
    def dot(l1, l2):
        assert len(l1) == len(l2)
        sum = 0
        for i in range(len(l1)):
            sum += l1[i] * l2[i]        
        return sum

    @staticmethod
    def eye(no_rows, no_cols):
        l = []
        for i in range(no_rows):
            l1 = [ 0 for i in range(no_cols) ]
            l1[i]= 1
            l.append(l1)
        return l
