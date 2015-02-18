# showkey -- keystroke echoer

VERS=$(shell sed <showkey.spec -n -e '/Version: \(.*\)/s//\1/p')

showkey: showkey.c
	$(CC) -DREVISION=$(VERS) showkey.c -o showkey

showkey.1: showkey.xml
	xmlto man showkey.xml 

showkey.html: showkey.xml
	xmlto html-nochunks showkey.xml 

clean:
	rm -f showkey showkey.o splashscreen.h showkey.1 *.rpm *.tar.gz
	rm -f MANIFEST SHIPPER.*

install: showkey.1
	cp showkey /usr/bin/showkey
	cp showkey.1 /usr/share/man/man1

uninstall:
	rm /usr/bin/showkey
	rm /usr/share/man/man1/showkey.1

SOURCES = README COPYING Makefile showkey.c showkey.xml showkey.spec

showkey-$(VERS).tar.gz: $(SOURCES) showkey.1
	@ls $(SOURCES) showkey.1 | sed s:^:showkey-$(VERS)/: >MANIFEST
	@(cd ..; ln -s showkey showkey-$(VERS))
	(cd ..; tar -czf showkey/showkey-$(VERS).tar.gz `cat showkey/MANIFEST`)
	@(cd ..; rm showkey-$(VERS))

dist: showkey-$(VERS).tar.gz

release: showkey-$(VERS).tar.gz showkey.html
	shipper -u -m -t; make clean
