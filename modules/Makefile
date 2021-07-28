ACTONC=../actonc
all: __builtin__.ty math.ty numpy.ty

__builtin__.ty: __builtin__.act
	$(ACTONC) --stub __builtin__.act

math.ty: math.act __builtin__.ty
	$(ACTONC) --stub math.act

time.ty: time.act __builtin__.ty
	$(ACTONC) --stub time.act

numpy.ty: numpy.act math.ty
	$(ACTONC) --stub numpy.act

clean:
	rm -f *.ty
