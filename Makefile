VPATH = manual
CUBISM_VERSION_FILE = cubism/main/cubism_version.pro
CUBISM-VERSION := $(shell perl -ne 'print $$1 if /cubism_version='"'"'(v[0-9.a-z]+)/;' $(CUBISM_VERSION_FILE))

CUBISM_BIN_FILE = cubism_$(CUBISM-VERSION)_bin.tgz
CUBISM_TAR_FILE = cubism_$(CUBISM-VERSION)_src.tar
CUBISM_SRC_FILE = cubism_$(CUBISM-VERSION)_src.tgz
CUBISM_SAV_FILE = cubism/bin/cubism_vm.sav
CUBISM_MAN_FILE = manual/cubism.pdf
CUBISM_APP = Cubism.app
CUBISM_DMG_DIR =  cubism_$(CUBISM-VERSION)
CUBISM_DMG = $(CUBISM_DMG_DIR).dmg

IDL = idl

.PHONY: all
all: bindist srcdist dmg

.PHONY: srcdist
srcdist: $(CUBISM_SRC_FILE) 

.PHONY: bindist
bindist: $(CUBISM_BIN_FILE)

.PHONY: savefile
savefile: $(CUBISM_SAV_FILE)

.PHONY: manual
manual: $(CUBISM_MAN_FILE)

.PHONY: dmg
dmg: $(CUBISM_DMG) $(CUBISM_APP_FILE)

.PHONY: app
app: $(CUBISM_APP)

$(CUBISM_BIN_FILE): $(CUBISM_SAV_FILE) $(CUBISM_MAN_FILE)
	tar czvf $(CUBISM_BIN_FILE) --exclude ".git" \
		--exclude '.??*' --exclude '*~' --exclude '#*#' \
		README \
		cubism/bin/cubism_vm* cubism/calib/data cubism/calib/*.c \
		cubism/bin/irs_info cubism/map_sets manual/cubism.pdf

$(CUBISM_SAV_FILE): $(CUBISM_VERSION_FILE) 
	(cd cubism/bin; $(IDL) do_compile_cubism)

$(CUBISM_MAN_FILE): manual/cubism.texi
	(cd manual; make pdf)

$(CUBISM_SRC_FILE): $(CUBISM_MAN_FILE) 
	git archive --format=tar --prefix=irs_cubism/ HEAD \
		CHANGES COPYING README cubism/ manual/ \
		objtools/ tvtools/ utility/ > $(CUBISM_TAR_FILE)
	(cd ../; tar -rvf irs_cubism/$(CUBISM_TAR_FILE) \
		irs_cubism/manual/cubism.pdf )
	gzip -c $(CUBISM_TAR_FILE) > $(CUBISM_SRC_FILE)
	rm -f $(CUBISM_TAR_FILE)

$(CUBISM_APP): $(CUBISM_SAVE_FILE) $(CUBISM_MAN_FILE)
	rm -rf $(CUBISM_APP)
	platypus -D -a 'Cubism' -o 'None' -u 'JD Smith' \
		-p /bin/sh -V $(CUBISM-VERSION) -I org.jdsmith.Cubism \
		-s Cbsm -X 'cpj' -T Cbsm \
		-i cubism/main/cubism.icns \
		-f $(CUBISM_SAV_FILE) -f cubism/calib/data \
		-f cubism/calib/polyclip.c -f cubism/map_sets \
		-f cubism/bin/cubism_vm -f manual/cubism.pdf \
		-c cubism/bin/cubism_vm_platypus \
		./$(CUBISM_APP)
	cp -f cubism/main/CubismFiles.icns $(CUBISM_APP)/Contents/Resources/
	(cd $(CUBISM_APP)/Contents/Resources/;	mkdir manual; \
		mv -f cubism.pdf manual/; mkdir bin; \
		mv -f cubism_vm cubism_vm.sav bin/; \
		mkdir calib; mv -f data polyclip.c calib/; \
		mkdir cubism; mv -f map_sets calib bin cubism/)
	perl -i -pe '$$i=/cpj/..m|</array>|; $$_= \
"        		<key>CFBundleTypeIconFile</key>\n" . \
"        		        <string>CubismFiles.icns</string>\n" .\
"        		<key>CFBundleTypeOSTypes</key>\n" .\
"        		<array>\n".\
"        		        <string>Cbsm</string>\n" .\
"        		</array>\n" . $$_ if $$l && !$$i; $$l=$$i;' \
		$(CUBISM_APP)/Contents/Info.plist 
	cp -f cubism/main/CubismFiles.icns $(CUBISM_APP)/Contents/

$(CUBISM_DMG): $(CUBISM_APP) $(CUBISM_SAV_FILE)
	rm -rf "$(CUBISM_DMG_DIR)"
	mkdir "$(CUBISM_DMG_DIR)"
	cp -rp "$(CUBISM_APP)" "$(CUBISM_DMG_DIR)"
	cp README "$(CUBISM_DMG_DIR)"
	rm -f "$(CUBISM_DMG)"
	hdiutil create -fs HFS+ -srcfolder "$(CUBISM_DMG_DIR)" \
		-volname "Cubism $(CUBISM-VERSION)" "$(CUBISM_DMG)"
	rm -rf "$(CUBISM_DMG_DIR)"
