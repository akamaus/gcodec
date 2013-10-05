HSOURCE=VarMap.hs HCode.hs Geometry.hs Expr.hs  GCode.hs AwePrelude.hs

PROG=gcode

$(PROG): ProductionTest
	./ProductionTest > $(PROG) 2> build_errlog && echo "all done" >> build_errlog

ProductionTest: ProductionTest.hs $(HSOURCE)
	echo "compiling.." > build_errlog
	ghc --make ProductionTest.hs >build_log 2>build_errlog
