source=code/parser.rkt code/handlers.rkt code/handler_basic.rkt code/fetzen.rkt
manual=doc/fetzen_manual.pdf


code/%.rkt: src/%.rkt
	./fetzen -c $@ -d $(@:code/%=doc/%) --handler latex $<

fetzen: $(source)
	raco exe -o fetzen code/fetzen.rkt

fetzen_bootstrap:
	raco exe -o fetzen code/fetzen.rkt

test: fetzen
	racket test/run.rkt

clean:
	\rm -f fetzen	
	\rm -f $(manual)
	\rm -f $(manual:%.pdf=%.log)
	\rm -f $(manual:%.pdf=%.aux)
	\rm -f doc/*rkt

doc: $(manual)

$(manual): $(manual:%.pdf=%.tex) $(source:code/%.rkt=doc/%.rkt)
	cd $(shell dirname $(manual)) ; \
	pdflatex $(shell basename $(manual:%.pdf=%.tex))

doc/%.rkt: src/%.rkt
	./fetzen -c $@ -d $(@:code/%=doc/%) --handler latex $<

all: fetzen
