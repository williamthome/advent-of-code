input:
	@$(call run,input)

example:
	@$(call run,example)

define run
	$(eval $@_FILE = $(1))
	rebar3 compile
	erl -noshell \
	    -pa ./_build/default/lib/day1/ebin/ \
		-eval "io:format(\"Result: ~p~n\", [day1:file(\"${$@_FILE}\")]), halt(0)"
endef
