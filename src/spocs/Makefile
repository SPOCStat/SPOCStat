# Automatic Fortran dependency resolution using makedepf90:
# http://personal.inet.fi/private/erikedelmann/makedepf90
# Requires: bash, *nix, GNU Make
# Tested with: Intel Composer XE 2013 Fortran compiler (ifort)
# TODO: clean up/abstract
#	configure other compilers

DEPDIR = .deps
df = $(DEPDIR)/$(*F)
SOURCES = $(notdir $(wildcard *.[fF]90 ../common/*.[F]90))
VPATH = ../common/ $(DEPDIR)
FC = ifort
FSYNTAXFLAG = -DBIGINT=8 -syntax-only -warn -stand f08 -module $(DEPDIR)
FPPFLAGS = -fpp
MAKEDEPF90 = makedepf90
FORTRAN_INTRINSIC_MODULES = iso_c_binding iso_fortran_env ieee_arithmetic ieee_exceptions ieee_features
INTRINSIC_MODULES = $(foreach module,$(FORTRAN_INTRINSIC_MODULES), -u $(module))

$(DEPDIR):
	mkdir $(DEPDIR)

# This is a bit of a mess, but I can't figure out a better way
$(DEPDIR)/%.d : %.F90 Makefile | $(DEPDIR)
	@set -e; rm -f $@; mkfifo $@.fifo.$$$$; \
	sed -n "s,^.*\'\(.*\)\'.*$$,\1.mod,p" < $@.fifo.$$$$ | tr '\n' ' ' | \
	tr [A-Z] [a-z] > $@.moddep.$$$$ & \
	echo -e '\n' >> $@.moddep.$$$$; \
	$(MAKEDEPF90) $(INTRINSIC_MODULES) -W $< 2> $@.fifo.$$$$ > $@.$$$$; \
	sed 's,\([^ :]*$*\)\.o,$@,' < $@.$$$$ > $@; \
	$(MAKEDEPF90) $(INTRINSIC_MODULES) -m %m.mod $< > $@.$$$$; \
	sed 's,[^ :]*\($*\)\.o,$(DEPDIR)/\1.syntax,' < $@.$$$$ | \
	sed 's,\([^ ]*\.mod\),$(DEPDIR)/\1,' | tr '\n' ' ' | cat - $@.moddep.$$$$ >> $@; \
	cp $@ $@.$$$$; \
	echo 'Makefile\n\t$$(FC) $$(FSYNTAXFLAG) $$(FPPFLAGS) $$< 2> $$(<:.F90=.syntax) && \\' >> $@; \
	echo '\t(([ -f $$(notdir $$(<:%.F90=%.i90)) ] && mv $$(notdir $$(<:%.F90=%.i90)) $(DEPDIR)) || true ); \\' >> $@; \
	echo '\t[ -f $$(<:.F90=.syntax) ] && mv $$(<:.F90=.syntax) $(DEPDIR) ;' >> $@; \
	sed -e '1,2d' -e 's,#.*,,' -e 's,^[^:]*: *,,' -e 's, *\\$$,,' \
                -e '/^$$/ d' -e 's,$$, :,' < $@.$$$$ >> $@; \
	rm -f $@.$$$$; rm -f $@.fifo.$$$$; rm -f $@.moddep.$$$$;

-include $(SOURCES:%.F90=$(DEPDIR)/%.d)

.PHONY: check-syntax
check-syntax: $(DEPDIR)/$(notdir $(CHK_SOURCES:%.F90=%.syntax))
	cat $(DEPDIR)/$(notdir $(CHK_SOURCES:%.F90=%.syntax)) | sed 's,$(notdir $(CHK_SOURCES)),$(CHK_SOURCES),' >&2
