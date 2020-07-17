AM_FOO = esy/platform-darwin/_build/default

install: esy esy-solve-cudf
	install -d $(libdir)/bin
	install -m755 $(AM_FOO)/bin/esy.exe $(libdir)/bin/esy.exe
	install -d $(libdir)/esy-build-package $(libdir)/esy-build-package
	install -m755 $(AM_FOO)/esy-build-package/bin/esyBuildPackageCommand.exe  $(libdir)/esy-build-package/esyBuildPackageCommand.exe
	install -m755 $(AM_FOO)/esy-build-package/bin/esyRewritePrefixCommand.exe  $(libdir)/esy-build-package/esyRewritePrefixCommand.exe
	install -m744 esy/package.json $(libdir)

	install -d esy-solve-cudf/platform-darwin/ $(libdir)/node_modules/esy-solve-cudf
	install -m755 esy-solve-cudf/platform-darwin/esySolveCudfCommand.exe $(libdir)/node_modules/esy-solve-cudf
	install -m744 esy-solve-cudf/package.json $(libdir)/node_modules/esy-solve-cudf

	mkdir $(bindir)
	$(LN_S) $(libdir)/bin/esy.exe $(bindir)/esy 

clean-local:
	@rm config.status configure config.log
	@rm Makefile
	@rm -r autom4te.cache/
	@rm aclocal.m4
	@rm compile install-sh missing Makefile.in
