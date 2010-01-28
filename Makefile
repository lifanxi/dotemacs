# The Makefile to prepare the emacs environment


all: cscope

.PHONY: cscope
cscope:
	tar xjvf cscope-15.7a.tar.bz2 -C /tmp
	( cd /tmp/cscope-15.7a ; \
	./configure; \
	make ; \
	sudo make install; \
	mkdir -pv ~/emacs/cscope; \
	cp /tmp/cscope-15.7a/contrib/xcscope/xcscope.el ~/emacs/cscope; \
	sudo cp /tmp/cscope-15.7a/contrib/xcscope/cscope-indexer /usr/local/bin; \
	 )