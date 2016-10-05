source=code/parser.rkt code/writer.rkt code/fetzen.rkt code/utils.rkt \
       code/preprocessors.rkt code/postprocessors.rkt code/data.rkt
manual=doc/fetzen_manual.pdf


code/%.rkt: src/%.rkt fetzen_bootstrap
	mkdir -p code
	./fetzen --out $@,code --out $(@:code/%=doc/%),docu-latex \
	    --preprocessor uncomment-double-semicolon $<

fetzen: $(source)
	raco exe -o fetzen code/fetzen.rkt

fetzen_bootstrap:
	raco exe -o fetzen src/fetzen.rkt

test: fetzen
	racket test/run.rkt

clean:
	\rm -f fetzen	
	\rm -f $(manual)
	\rm -f $(manual:%.pdf=%.log)
	\rm -f $(manual:%.pdf=%.aux)
	\rm -f doc/*rkt
	\rm -f code/*rkt

doc: $(manual)

$(manual): $(manual:%.pdf=%.tex) $(source:code/%.rkt=doc/%.rkt)
	cd $(shell dirname $(manual)) ; \
	pdflatex $(shell basename $(manual:%.pdf=%.tex))

doc/%.rkt: src/%.rkt
	./fetzen --out $@,code --out $(@:code/%=doc/%),docu-latex \
	    --preprocessor uncomment-double-semicolon $<

all: fetzen
