checker.escript: ebin/checker.beam
	printf '#!escript/n' > header
	cat header ebin/checker.beam > checker.escript
	rm header
	chmod +x checker.escript
ebin/checker.beam: src/checker.erl
	erlc -o ebin src/checker.erl
clean:
	rm checker.escript
