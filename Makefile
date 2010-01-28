# The Makefile to prepare the emacs environment


all: cscope


cscope:
	tar xjvf cscope-15.7a.tar.bz2 -C /tmp
	( cd /tmp/cscope-15.7a ; \
	./configure --prefix=$(HOME)/emacs ; \
	make ; \
	make install )