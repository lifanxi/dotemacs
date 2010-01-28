# The Makefile to prepare the emacs environment


all: cscope

.PHONY: cscope
cscope:
	tar xjvf cscope-15.7a.tar.bz2 -C /tmp
	( cd /tmp/cscope-15.7a ; \
	./configure; \
	make ; \
	mkdir -pv ~/emacs/cscope; \
	 )

.PHONY: install
install: cscope
	( \
	cd /tmp/cscope-15.7a ; \
	make install; \
	cp /tmp/cscope-15.7a/contrib/xcscope/xcscope.el ~/emacs/cscope; \
	cp /tmp/cscope-15.7a/contrib/xcscope/cscope-indexer /usr/local/bin; \
	cp ~/emacs/p4 /usr/local/bin/ ;\
	chmod a+x /usr/local/bin/p4 \
    )

