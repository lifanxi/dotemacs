CEDET_FILE=cedet-1.0pre7
MUSE_FILE=muse-3.20
EMACS_CONF_PATH=~/emacs

all: cedet muse post

cedet: $(CEDET_FILE).tar.gz
	tar xzvf $(CEDET_FILE).tar.gz -C $(EMACS_CONF_PATH)
	mv $(EMACS_CONF_PATH)/$(CEDET_FILE) $(EMACS_CONF_PATH)/cedet
	(cd $(EMACS_CONF_PATH)/cedet && $(MAKE))
	mkdir -p ~/.srecode

muse: $(MUSE_FILE).tar.gz
	tar xzvf $(MUSE_FILE).tar.gz -C /tmp
	cp muse.Makefile.defs /tmp/$(MUSE_FILE)/Makefile.defs
	(cd /tmp/$(MUSE_FILE) && make && make install-bin)

post:
	ln -sf ~/emacs/.emacs ~/.emacs

clean:
	rm -rf $(EMACS_CONF_PATH)/cedet
	rm -rf $(EMACS_CONF_PATH)/muse

# Download files

$(MUSE_FILE).tar.gz: 
	wget http://download.gna.org/muse-el/muse-3.20.tar.gz

$(CEDET_FILE).tar.gz:
	wget http://downloads.sourceforge.net/project/cedet/cedet/1.0pre7/cedet-1.0pre7.tar.gz?use_mirror=cdnetworks-us-2