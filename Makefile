## DARKSIDER ##

# Libraries
LIBS=unix.cma str.cma 

# Sources
SOURCES=utils.ml Petri_nets.ml formulas.ml PN_to_formula.ml logs.ml alignment_SAT.ml fullrunClustering_PB.ml subnetClustering_PB.ml  alignment_PB_newEditDistance.ml anti_alignment_PB_newEditDistance.ml solve_formulas_PB.ml alignment_PB.ml  main.ml
SOURCESDIR=src

CMO=$(SOURCES:.ml=.cmo)

# flags 
FLAGS=-c -w -5

# Executable' name
EXEC=darksider


exe: $(CMO)
	@ocamlc -o $(EXEC) $(LIBS) $(addprefix $(SOURCESDIR)/,$^)
	@echo "./darksider -help to see manual"
	

%.cmo: $(SOURCESDIR)/%.ml
	@ocamlc $(FLAGS) -I $(SOURCESDIR) $< 

clean:
	rm -f $(SOURCESDIR)/*.cmo $(SOURCESDIR)/*.cmi sol alignment.pb $(EXEC)
