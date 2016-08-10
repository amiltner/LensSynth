CFLAGS := -w,@A,-w,-4,-w,-44,-w,-48,-w,-26
OCBFLAGS := -cflags $(CFLAGS)
OCB := ocamlbuild $(OCBFLAGS)

.PHONY: all debug clean top profile gen-baselines check-baselines test

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

top: cmdline.cma
	utop

CHECK_BASELINE := python check-baseline.py

check-baselines: cmdline.native
	$(CHECK_BASELINE) ./cmdline.native tests/

GENERATE_DATA := python generate-data.py

generate-data: synml.native
	@$(GENERATE_DATA) ./cmdline.native tests/

profile: synml_profile.p.native
	instruments -t "/Applications/Xcode.app/Contents/Applications/Instruments.app/Contents/Resources/templates/Time Profiler.tracetemplate" cmdline_profile.p.native

test:
	$(OCB) unittests.native
	./unittests.native
