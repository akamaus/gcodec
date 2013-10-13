HSOURCE=VarMap.hs HCode.hs Geometry.hs Expr.hs  GCode.hs AwePrelude.hs

SOURCE=ProductionTest.hs
PROG=gcode

$(PROG): fanuc-macro-generator
	./fanuc-macro-generator > $(PROG) 2> build_errlog && echo "all done" >> build_errlog

fanuc-macro-generator: $(SOURCE) $(HSOURCE)
	echo "compiling.." > build_errlog
	ghc --make -o fanuc-macro-generator $(SOURCE)  >build_log 2>build_errlog
