VPATH = manual
CUBISM_VERSION_FILE = cubism/main/cubism_version.pro
CUBISM-VERSION := $(shell perl -ne 'print $$1 if /cubism_version='"'"'(v[0-9.a-z]+)/;' $(CUBISM_VERSION_FILE))

CUBISM_BIN_FILE = cubism_$(CUBISM-VERSION)_bin.tgz
CUBISM_TAR_FILE = cubism_$(CUBISM-VERSION)_src.tar
CUBISM_SRC_FILE = cubism_$(CUBISM-VERSION)_src.tgz
CUBISM_SAV_FILE = cubism/bin/cubism_vm.sav

IDL = idl

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
	(cd manual; make pdf)

$(CUBISM_BIN_FILE): $(CUBISM_SAV_FILE)
	tar czvf $(CUBISM_BIN_FILE) --exclude ".git" \
		--exclude '.??*' --exclude '*~' --exclude '#*#' \
		README \
		cubism/bin/cubism_vm* cubism/calib/data cubism/calib/*.c \
		cubism/bin/irs_info cubism/map_sets manual/cubism.pdf

$(CUBISM_SAV_FILE): $(CUBISM_VERSION_FILE)
	(cd cubism/bin; $(IDL) do_compile_cubism)

$(CUBISM_SRC_FILE): 
	git archive --format=tar --prefix=irs_cubism/ HEAD \
		CHANGES COPYING README cubism/ manual/ \
		objtools/ tvtools/ utility/ > $(CUBISM_TAR_FILE)
	(cd ../; tar -rvf irs_cubism/$(CUBISM_TAR_FILE) \
		irs_cubism/manual/cubism.pdf )
	gzip -c $(CUBISM_TAR_FILE) > $(CUBISM_SRC_FILE)
	rm -f $(CUBISM_TAR_FILE)



 
