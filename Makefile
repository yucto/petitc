SOURCES := src/ gmrs/petitc.lx gmrs/petitc.gr Makefile Cargo.toml flake.nix \
	run-tests docs/
USEFUL_ARTIFACTS := Cargo.lock flake.lock gmrs/petitc.clx gmrs/petitc.cgr
STEPS := 1b 2b
ifdef RELEASE
COMPILER := ./petitc
else
COMPILER := out/petitc
endif

LATEX ?= lualatex
LATEX_FLAGS ?= -shell-escape -interaction=batchmode -output-directory=out

all: petitc

archive: out/caspar-mathieu.tgz

build: out/petitc

clean:
	cargo clean
	$(RM) -r target/ out/ ./petitc

distclean: clean
	$(RM) $(USEFUL_ARTIFACTS)

doc: out/rapport.pdf

fmt:
	@cargo fmt

test: $(COMPILER)
	$(foreach step,$(STEPS),./run-tests -$(step) $(COMPILER);)

out:
	@mkdir $@

out/petitc: target/debug/petitc | out
	@cp $< $@

gmrs/%.clx: gmrs/%.lx
	beans compile lexer -o $@ $<

gmrs/%.cgr: gmrs/%.gr gmrs/%.clx
	beans compile parser --lexer $(word 2,$^) -o $@ $<

out/caspar-mathieu.tgz: $(SOURCES) $(USEFUL_ARTIFACTS) | out
	@tar -czvf $@ $^ --transform 's,^,caspar-mathieu/,'

out/%.pdf out/%.aux out/%.log &: docs/%.tex | out
	$(LATEX) $(LATEX_FLAGS) $<

petitc: target/release/petitc
	@cp $< $@

target/release/petitc: src/ gmrs/petitc.clx gmrs/petitc.cgr
	cargo build --release

target/debug/petitc: src/ gmrs/petitc.clx gmrs/petitc.cgr
	cargo build

.PHONY: archive all build clean distclean doc fmt tests
.PRECIOUS: gmrs/petitc.clx gmrs/petitc.cgr
