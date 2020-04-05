#TOOLS
BIGLOO = bigloo
BGLAFILE = bglafile
BGLJFILE = bgljfile
BGLTAGS = bgltags
LD = gcc --shared
AR = ar
JAR = jar
RANLIB = ranlib
INSTALL = install
LINK = ln
MAKEINFO = makeinfo
space:=$(shell echo " ")
colon:=:
epound:=\#
pound:=$(shell echo '$(epound)')

BIGLOO_VERSION := $(shell $(BIGLOO) -eval "(print *bigloo-version*) (exit 0)" -q)
BIGLOOLIBDIR := $(shell $(BIGLOO) -eval "(print *default-lib-dir*) (exit 0)" -q)


#install related variables
DESTDIR = /usr
INSTLIBDIR = $(DESTDIR)/lib
INSTBIGLOOLIBDIR = $(INSTLIBDIR)/bigloo/$(BIGLOO_VERSION)

#C Flags
CCOMMONFLAGS = -fPIC

#Bigloo Flags
BHEAPFLAGS = -I src/Llib -I $(GENSCMDIR) -unsafe -q -mkaddheap -mkaddlib \
	     -heap-library $(LIBNAME)

BCOMMONFLAGS = -fisa -mkaddlib -fsharing -q \
               -copt '$(CCOMMONFLAGS)' -I src/Llib -I $(GENSCMDIR)

BSAFEFLAGS = $(BCOMMONFLAGS) -O6 

BPROFILEFLAGS = $(BCOMMONFLAGS) -p -O6

BUNSAFEFLAGS = $(BCOMMONFLAGS) -Obench


# directory variables
OBJDIR = objs
DISTDIR = dist
SCLASSDIR = classes_s/bigloo/$(LIBNAME)
UCLASSDIR = classes_u/bigloo/$(LIBNAME)
ESCLASSDIR = classes_es/bigloo/$(LIBNAME)
EUCLASSDIR = classes_eu/bigloo/$(LIBNAME)
GENSCMDIR = src/Llib/genenerated


# defines
define generateArrayScm =
$(GENSCMDIR)/$(1)array.scm: src/Llib/array.scm.in $(GENSCMDIR)/$(1)array.sch
	@if [ "$(1) " = " " ]; then \
	  sed -e "s|@type-tag@|$(1)|g" \
	      -e "s|@type-default@|$(2)|g" \
	      -e "s|@store-ref@|store-ref|g" \
	      -e "s|@store-set@|store-set!|g" \
	      -e "s|@type@|$(call lookup,$(1),$(TYPETAGS2TYPES))|g" \
	      -e "s|@type-import@||g" $$^ > $$@ ;\
	else \
	  sed -e "s|@type-tag@|$(1)|g" \
	      -e "s|@type-default@|$(2)|g" \
	      -e "s|@store-ref@|$(1)vector-ref|g" \
	      -e "s|@store-set@|$(1)vector-set!|g" \
	      -e "s|@type@|$(call lookup,$(1),$(TYPETAGS2TYPES))|g" \
	      -e "s|@type-import@|srfi25/array|g" $$^ > $$@ ;\
	fi

endef

define generateArraySch =
$(GENSCMDIR)/$(1)array.sch: src/Llib/array.sch.in
	@sed -e "s|@type-tag@|$(1)|g" \
	-e "s|@type-default@|$(2)|g" \
	$$^ > $$@

endef


define left =
$(strip $(subst dummy,,$(word 1,$(subst _,$(space),$1))))
endef

define right =
$(strip $(word 2,$(subst _,$(space),$1)))
endef

define lookup =
$(if $(1),$(call right, $(filter $(1)_%, $(2))),$(call right, $(filter dummy_%, $(2))))
endef

# sources
TYPETAGS2TYPES = dummy_obj s8_int8 u8_uint8 s16_int16 u16_uint16 s32_int32 u32_uint32 s64_int64 u64_uint64 f32_float f64_double
TYPETAGS2DEFAULTS = dummy_$(pound)unspecified s8_$(pound)s8$(colon)0  u8_$(pound)u8$(colon)0 s16_$(pound)s16$(colon)0  u16_$(pound)u16$(colon)0 s32_$(pound)s32$(colon)0 u32_$(pound)u32$(colon)0 s64_$(pound)s64$(colon)0 u64_$(pound)u64$(colon)0 f32_0.0 f64_0.0

GENERATEDINCLUDES = $(foreach tag, $(TYPETAGS2DEFAULTS), $(GENSCMDIR)/$(call left, $(tag))array.sch)
GENERATEDSRCS =  $(foreach tag, $(TYPETAGS2DEFAULTS), $(GENSCMDIR)/$(call left, $(tag))array.scm)	
BIGLOOSRCS = $(wildcard src/Llib/*.scm) $(GENERATEDSRCS)
TESTSRCS = $(wildcard recette/*.scm)
HEAPSRC = src/Misc/make_lib.scm
DOCSRCS = $(wildcard manual/*.texi)

# object files
_TESTSRCSWOUTDIR = $(foreach src, $(TESTSRCS), $(notdir $(src)))
_SRCSWOUTDIR = $(foreach src, $(BIGLOOSRCS), $(notdir $(src)))
_BASEOBJSWOUTDIR = $(_SRCSWOUTDIR:%.scm=%)

SAFEOBJECTS = $(_BASEOBJSWOUTDIR:%=$(OBJDIR)/%_s.o)

PROFILEOBJECTS = $(_BASEOBJSWOUTDIR:%=$(OBJDIR)/%_p.o)

UNSAFEOBJECTS = $(_BASEOBJSWOUTDIR:%=$(OBJDIR)/%_u.o)

SAFECLASSES = $(_BASEOBJSWOUTDIR:%=$(SCLASSDIR)/%.class)

UNSAFECLASSES = $(_BASEOBJSWOUTDIR:%=$(UCLASSDIR)/%.class)


_ESRCSWOUTDIR = $(foreach src, $(HEAPSRC), $(notdir $(src)))
_EBASEOBJSWOUTDIR = $(_ESRCSWOUTDIR:%.scm=%)

ESAFEOBJECTS = $(_EBASEOBJSWOUTDIR:%=$(OBJDIR)/%_s.o)

EUNSAFEOBJECTS = $(_EBASEOBJSWOUTDIR:%=$(OBJDIR)/%_u.o)

ESAFECLASSES = $(_EBASEOBJSWOUTDIR:%=$(ESCLASSDIR)/%.class)

EUNSAFECLASSES = $(_EBASEOBJSWOUTDIR:%=$(EUCLASSDIR)/%.class)

TESTOBJECTS = $(TESTSRCS:%.scm=%.o)

TESTCLASSES = $(TESTSRCS:%.scm=%.class)

.SUFFIXES:

$(OBJDIR)/%_s.o:src/Llib/%.scm
	$(BIGLOO)  -c $(BSAFEFLAGS) -o $@ $^

$(OBJDIR)/%_p.o:src/Llib/%.scm
	$(BIGLOO)  -c $(BPROFILEFLAGS) -o $@ $^

$(OBJDIR)/%_u.o:src/Llib/%.scm
	$(BIGLOO)  -c $(BUNSAFEFLAGS) -o $@ $^

$(OBJDIR)/%_s.o:$(GENSCMDIR)/%.scm
	$(BIGLOO)  -c $(BSAFEFLAGS) -o $@ $^

$(OBJDIR)/%_p.o:$(GENSCMDIR)/%.scm
	$(BIGLOO)  -c $(BPROFILEFLAGS) -o $@ $^

$(OBJDIR)/%_u.o:$(GENSCMDIR)/%.scm
	$(BIGLOO)  -c $(BUNSAFEFLAGS) -o $@ $^


$(SCLASSDIR)/%.class:src/Llib/%.scm
	$(BIGLOO) -jvm  -c $(BSAFEFLAGS) -o $@ $^

$(UCLASSDIR)/%.class:src/Llib/%.scm
	$(BIGLOO) -jvm  -c $(BUNSAFEFLAGS) -o $@ $^

$(SCLASSDIR)/%.class:$(GENSCMDIR)/%.scm
	$(BIGLOO) -jvm  -c $(BSAFEFLAGS) -o $@ $^

$(UCLASSDIR)/%.class:$(GENSCMDIR)/%.scm
	$(BIGLOO) -jvm  -c $(BUNSAFEFLAGS) -o $@ $^


recette/%.o:recette/%.scm heap
	$(BIGLOO)  -c $(BSAFEFLAGS) -L $(DISTDIR) -o $@ $<

recette/%.class:recette/%.scm heap
	$(BIGLOO) -jfile .recettejfile -jvm  -c $(BSAFEFLAGS) -L $(DISTDIR) -o $@ $<


#targets

c: $(DISTDIR) $(GENSCMDIR) .afile .etags lib 

jvm: $(DISTDIR) $(GENSCMDIR) .jfile .afile .etags libj

.PHONY: all 
all: c jvm

$(foreach tagdefault,$(TYPETAGS2DEFAULTS),$(eval $(call generateArrayScm,$(call left, $(tagdefault)),$(call right, $(tagdefault)))))

$(foreach tagdefault,$(TYPETAGS2DEFAULTS),$(eval $(call generateArraySch,$(call left, $(tagdefault)),$(call right, $(tagdefault)))))


test: c recette/test$(LIBNAME) 
	./recette/test$(LIBNAME)

jtest: jvm .recettejfile recette/jtest$(LIBNAME) 
	./recette/jtest$(LIBNAME)

manual: manual-html manual-pdf

manual-html : $(DOCSRCS)
	(cd manual; \
	makeinfo --html $(LIBNAME).texi)

manual-pdf : $(DOCSRCS)
	(cd manual; \
	makeinfo --pdf $(LIBNAME).texi)

.afile: $(BIGLOOSRCS)  $(TESTSRCS)
	$(BGLAFILE) -o $@  $^

.jfile: $(BIGLOOSRCS)
	$(BGLJFILE) -pbase bigloo.$(LIBNAME) -o $@  -I recette -I src/Misc -I src/Llib -I $(GENSCMDIR) $(_SRCSWOUTDIR)

.recettejfile: $(TESTSRCS)
	$(BGLJFILE) -pbase recette -o $@  -I recette  $(_TESTSRCSWOUTDIR)

.etags: $(BIGLOOSRCS) $(TESTSRCS) $(wildcard src/Llib/*.sch)
	$(BGLTAGS) -o $@ $^

heap: $(DISTDIR)/$(LIBNAME).heap

jheap: $(DISTDIR)/$(LIBNAME).jheap 

$(DISTDIR)/$(LIBNAME).heap: $(DISTDIR) $(HEAPSRC)
	$(BIGLOO) $(BHEAPFLAGS) $(HEAPSRC)  -addheap $@

$(DISTDIR)/$(LIBNAME).jheap: $(DISTDIR) $(HEAPSRC)
	$(BIGLOO) -jvm  $(BHEAPFLAGS) $(HEAPSRC)  -addheap $@

$(OBJDIR)/make_lib_s.o:src/Misc/make_lib.scm
	$(BIGLOO) -c  $(BSAFEFLAGS) -o $@ $^

$(ESCLASSDIR)/make_lib.class:src/Misc/make_lib.scm
	$(BIGLOO) -jvm  -c $(BSAFEFLAGS) -o $@ $^


$(OBJDIR)/make_lib_u.o:src/Misc/make_lib.scm
	$(BIGLOO) -c $(BUNSAFEFLAGS) -o $@ $^

$(EUCLASSDIR)/make_lib.class:src/Misc/make_lib.scm
	$(BIGLOO) -jvm -c $(BUNSAFEFLAGS) -o $@ $^



lib: init heap $(DISTDIR)/lib$(LIBNAME)_s-$(VERSION).so  \
	       $(DISTDIR)/lib$(LIBNAME)_s-$(VERSION).a   \
               $(DISTDIR)/lib$(LIBNAME)_p-$(VERSION).so  \
	       $(DISTDIR)/lib$(LIBNAME)_p-$(VERSION).a   \
               $(DISTDIR)/lib$(LIBNAME)_es-$(VERSION).so \
	       $(DISTDIR)/lib$(LIBNAME)_u-$(VERSION).so  \
	       $(DISTDIR)/lib$(LIBNAME)_u-$(VERSION).a   \
	       $(DISTDIR)/lib$(LIBNAME)_eu-$(VERSION).so

libj: init jheap heap jar_s jar_u jar_es jar_eu

init: $(DISTDIR)/$(LIBNAME).init 

$(DISTDIR)/$(LIBNAME).init : $(GENERATEDINCLUDES) $(wildcard src/Llib/*.sch) src/Misc/$(LIBNAME).init.in
	cat $^ > $@

jar_s: $(SCLASSDIR)  $(DISTDIR) $(SAFECLASSES)
	$(JAR) -cf $(DISTDIR)/$(LIBNAME)_s-$(VERSION).zip -C $(SCLASSDIR)/../../ .

jar_u: $(UCLASSDIR) $(EUCLASSDIR) $(DISTDIR) $(UNSAFECLASSES)
	$(JAR) -cf $(DISTDIR)/$(LIBNAME)_u-$(VERSION).zip -C $(UCLASSDIR)/../../ .   

jar_es : $(ESCLASSDIR) $(DISTDIR) $(ESAFECLASSES)
	$(JAR) -cf $(DISTDIR)/$(LIBNAME)_es-$(VERSION).zip -C $(ESCLASSDIR)/../../ . 


jar_eu : $(UCLASSDIR) $(DISTDIR) $(EUNSAFECLASSES)
	$(JAR) -cf $(DISTDIR)/$(LIBNAME)_eu-$(VERSION).zip -C $(UCLASSDIR)/../../ .


$(DISTDIR)/lib$(LIBNAME)_s-$(VERSION).so: $(OBJDIR) $(DISTDIR) $(SAFEOBJECTS)
	$(LD)  -o $(DISTDIR)/lib$(LIBNAME)_s-$(VERSION).so $(SAFEOBJECTS) -lc -lm

$(DISTDIR)/lib$(LIBNAME)_s-$(VERSION).a : $(OBJDIR) $(DISTDIR) $(SAFEOBJECTS)
	$(AR) qcv $(DISTDIR)/lib$(LIBNAME)_s-$(VERSION).a $(SAFEOBJECTS);\
	$(RANLIB) $(DISTDIR)/lib$(LIBNAME)_s-$(VERSION).a

$(DISTDIR)/lib$(LIBNAME)_p-$(VERSION).so: $(OBJDIR) $(DISTDIR) $(PROFILEOBJECTS)
	$(LD) -o $(DISTDIR)/lib$(LIBNAME)_p-$(VERSION).so $(PROFILEOBJECTS) -lc -lm

$(DISTDIR)/lib$(LIBNAME)_p-$(VERSION).a : $(OBJDIR) $(DISTDIR) $(PROFILEOBJECTS)
	$(AR) qcv $(DISTDIR)/lib$(LIBNAME)_p-$(VERSION).a $(PROFILEOBJECTS);\
	$(RANLIB) $(DISTDIR)/lib$(LIBNAME)_p-$(VERSION).a

$(DISTDIR)/lib$(LIBNAME)_es-$(VERSION).so : $(OBJDIR) $(DISTDIR) $(ESAFEOBJECTS)
	$(LD)  -o $(DISTDIR)/lib$(LIBNAME)_es-$(VERSION).so $(ESAFEOBJECTS) -lc -lm

$(DISTDIR)/lib$(LIBNAME)_u-$(VERSION).so : $(OBJDIR) $(DISTDIR) $(UNSAFEOBJECTS)
	$(LD)  -o $(DISTDIR)/lib$(LIBNAME)_u-$(VERSION).so $(UNSAFEOBJECTS) -lc -lm

$(DISTDIR)/lib$(LIBNAME)_u-$(VERSION).a : $(OBJDIR) $(DISTDIR) $(UNSAFEOBJECTS)
	$(AR) qcv $(DISTDIR)/lib$(LIBNAME)_u-$(VERSION).a $(SAFEOBJECTS);\
	$(RANLIB) $(DISTDIR)/lib$(LIBNAME)_u-$(VERSION).a

$(DISTDIR)/lib$(LIBNAME)_eu-$(VERSION).so : $(OBJDIR) $(DISTDIR) $(EUNSAFEOBJECTS)
	$(LD)  -o $(DISTDIR)/lib$(LIBNAME)_eu-$(VERSION).so $(EUNSAFEOBJECTS) -lc -lm


recette/test$(LIBNAME) : $(TESTOBJECTS) lib 
	$(BIGLOO) -static-all-bigloo $(BUNSAFEFLAGS) -L $(DISTDIR) -o $@ $(TESTOBJECTS)

recette/jtest$(LIBNAME) : $(TESTCLASSES) libj 
	$(BIGLOO) -jfile .recettejfile -jvm -static-all-bigloo $(BUNSAFEFLAGS) -L $(DISTDIR) -o $@ $(TESTCLASSES) 



$(OBJDIR):
	mkdir $@

$(DISTDIR):
	mkdir $@

$(SCLASSDIR):
	mkdir -p $@

$(UCLASSDIR):
	mkdir -p $@

$(ESCLASSDIR):
	mkdir -p $@

$(EUCLASSDIR):
	mkdir -p $@

$(GENSCMDIR):
	mkdir -p $@


check-syntax: .afile
	$(BIGLOO) $(BCOMMONFLAGS) -afile .afile -syntax-check ${CHK_SOURCES}


install: c jvm
	for file in $(DISTDIR)/*; do \
	  $(INSTALL) $$file $(INSTBIGLOOLIBDIR)/`basename $$file`; \
	done; \

clean:
	rm -f -r $(GENSCMDIR);\
	rm -f $(TESTOBJECTS);\
	rm -f $(TESTCLASSES);\
	rm -f -r $(OBJDIR);\
	rm -f -r $(DISTDIR);\
	rm -f -r $(SCLASSDIR);\
	rm -f -r $(UCLASSDIR);\
	rm -f -r $(ESCLASSDIR);\
	rm -f -r $(EUCLASSDIR);\
	rm -f $(LIBNAME).heap;\
	rm -f $(LIBNAME).jheap;\
	rm -f $(LIBNAME).init;\
	rm -f .afile;\
	rm -f .jfile;\
	rm -f .recettejfile;\
	rm -f recette/jtest$(LIBNAME);\
	rm -f recette/test$(LIBNAME)



