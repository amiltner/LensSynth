CFLAGS := -w,+A,-w,-4,-w,-44,-w,-48,-w,-26
OCBFLAGS := -cflags $(CFLAGS)
OCB := ocamlbuild $(OCBFLAGS)

.PHONY: all debug clean top profile unittest functionaltest test

all: cmdline.native
debug: all cmdline.cma

%.cma: .FORCE
	$(OCB) $@

%.cmxa: .FORCE
	$(OCB) $@

%.native: .FORCE
	$(OCB) $@

%.p.native: .FORCE
	$(OCB) $@

%.byte: .FORCE
	$(OCB) $@

.FORCE:

clean:
	$(OCB) -clean
	rm -rf generated_data

top: cmdline.cma
	utop

CHECK_WITH_BOOMERANG := python check-with-boomerang.py

GENERATE_DATA := python generate-data.py

generate-data: cmdline.native
	$(GENERATE_DATA) ./cmdline.native tests/

profile: synml_profile.p.native
	instruments -t "/Applications/Xcode.app/Contents/Applications/Instruments.app/Contents/Resources/templates/Time Profiler.tracetemplate" cmdline_profile.p.native

unittest: unittests.native
	./unittests.native

functionaltest: cmdline.native
	$(CHECK_WITH_BOOMERANG) ./cmdline.native tests/

test: unittest functionaltest
