# Author Tony Wallace
EFT=deps/erlang_config_tamer

all: config_checked_okay
config_checked_okay: globals.config.etf ebin/globals.beam
	escript scripts/checker/ebin/checker.beam
globals.config.etf: globals.config ebin/globals.def.etf
	$(EFT)/config_check globals.config ebin/globals.def.etf
ebin/globals.beam: ../src/globals.erl
	erlc -o ebin $< 

ebin/globals.def.etf: src/globals.def $(EFT)/datadef.def
	$(EFT)/config_check $< $(EFT)/datadef.def
	mv src/*.etf ebin
.PHONY: clean
clean:
	rm  ebin/*.beam
	rm config_checked_okay


