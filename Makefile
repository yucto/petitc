SOURCES = src/ gmrs/petitc.lx gmrs/petitc.gr Makefile Cargo.toml flake.nix
USEFUL_ARTIFACTS = Cargo.lock flake.lock gmrs/petitc.clx gmrs/petitc.cgr
STEPS = 1b 2b
ifdef RELEASE
COMPILER = ./petitc
else
COMPILER = out/petitc
endif

all: petitc
build: out/petitc

out/petitc: target/debug/petitc
	@mkdir -p out
	@cp $< $@

gmrs/%.clx: gmrs/%.lx
	beans compile lexer -o $@ $<

gmrs/%.cgr: gmrs/%.gr gmrs/%.clx
	beans compile parser --lexer $(word 2,$^) -o $@ $<

out/caspar-mathieu.tgz: $(SOURCES) $(USEFUL_ARTIFACTS)
	@mkdir -p out/
	@tar -czvf $@ $^ --transform 's,^,caspar-mathieu/,'

petitc: target/release/petitc
	@cp $< $@

archive: out/caspar-mathieu.tgz

target/release/petitc: src/ gmrs/petitc.clx gmrs/petitc.cgr
	cargo build --release

target/debug/petitc: src/ gmrs/petitc.clx gmrs/petitc.cgr
	cargo build

clean:
	$(RM) -rf target/ out/

distclean: clean
	$(RM) $(USEFUL_ARTIFACTS)

tests: $(COMPILER)
	$(foreach step,$(STEPS),./run-tests -$(step) $(COMPILER);)

.PHONY: archive all clean distclean build tests target/release/petitc target/debug/petitc
.PRECIOUS: gmrs/petitc.clx gmrs/petitc.cgr
