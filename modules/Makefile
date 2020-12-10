all: __builtin__.ty math.ty numpy.ty

__builtin__.ty: __builtin__.act
	actonc --stub __builtin__.act 2> /dev/null

math.ty: math.act
	actonc --stub math.act 2> /dev/null

numpy.ty: numpy.act
	actonc --stub numpy.act 2> /dev/null

clean:
	rm *.ty
