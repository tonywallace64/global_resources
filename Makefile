# Author Tony Wallace
EFT=deps/erlang_config_tamer

all: globals.config.etf ebin/globals.beam
globals.config.etf: globals.config ebin/globals.def.etf
	$(EFT)/config_check globals.config ebin/globals.def.etf
ebin/%.beam: src/%.erl
	erlc -o ebin $< 

ebin/globals.def.etf: src/globals.def $(EFT)/datadef.def
	$(EFT)/config_check $< $(EFT)/datadef.def
	mv src/*.etf ebin
clean:
	rm  ebin/*.beam


