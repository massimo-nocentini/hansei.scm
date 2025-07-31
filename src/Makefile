
all: test wolfram ve

test:
	cd test && time csi -s test.scm

wolfram:
	cd test && time csi -s wolfram.scm

ve:
	cd test && time csi -s variable-elimination.scm


install:
	chicken-install -sudo
