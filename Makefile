VPATH = cubism/manual
CUBISM_VERSION_FILE = cubism/main/cubism_version.pro
CUBISM-VERSION := $(shell perl -ne 'print $$1 if /cubism_version='"'"'(v[0-9.a-z]+)/;' $(CUBISM_VERSION_FILE))

CUBISM_BIN_FILE = cubism_$(CUBISM-VERSION)_bin.tgz
CUBISM_SRC_FILE = cubism_$(CUBISM-VERSION)_src.tgz
CUBISM_SAV_FILE = cubism/bin/cubism_vm.sav

IDL = idl_6.3

.PHONY: all
all: manual bindist srcdist

.PHONY: srcdist
srcdist: $(CUBISM_SRC_FILE) 

.PHONY: bindist
bindist: $(CUBISM_BIN_FILE)

.PHONY: savefile
savefile: $(CUBISM_SAV_FILE)

.PHONY: manual
manual: cubism.pdf
	(cd cubism/manual; make pdf)

$(CUBISM_BIN_FILE): $(CUBISM_SAV_FILE) 
	tar czvf $(CUBISM_BIN_FILE) --exclude "CVS" \
		--exclude '.??*' --exclude '*~' --exclude '#*#' \
		README \
		cubism/bin/cubism_vm* cubism/calib/data cubism/calib/*.c \
		cubism/bin/irs_info cubism/map_sets cubism/manual/cubism.pdf

$(CUBISM_SAV_FILE): $(CUBISM_VERSION_FILE)
	(cd cubism/bin; $(IDL) do_compile_cubism)

$(CUBISM_SRC_FILE): ChangeLog 
	rm -rf irs_cubism_$(CUBISM-VERSION)
	mkdir irs_cubism_$(CUBISM-VERSION)
	cp README irs_cubism_$(CUBISM-VERSION)
	(cd irs_cubism_$(CUBISM-VERSION);\
	 cvs co cubism; \
	 rm -f cubism/TODO; \
	 rm -rf cubism/manual/*; \
	 cp ../cubism/manual/cubism.pdf cubism/manual/; \
	 cvs co objtools; \
	 cvs co tvtools)
	tar czvf $(CUBISM_SRC_FILE) --exclude 'CVS' irs_cubism_$(CUBISM-VERSION)
	rm -rf irs_cubism_$(CUBISM-VERSION)

ChangeLog: $(CUBISM_VERSION_FILE)
	cvs2cl  --no-wrap -S

 
