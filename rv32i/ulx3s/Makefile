
PROJ = risc-v
TOPENT = topEntity

pack:
	ecppack --compress --freq 62.0 --input ulx3s.config --bit $(PROJ).bit

config:
	nextpnr-ecp5 --timing-allow-fail --85k --package CABGA381 --json $(PROJ).json --lpf constraints.lpf --textcfg ulx3s.config 

json:
	yosys \
		-p "read -sv ../verilog/TopEntity/$(TOPENT).v" \
		-p "hierarchy -top $(TOPENT)" \
		-p "synth_ecp5  -json $(PROJ).json"

prog:
	fujprog $(PROJ).bit

all: json config pack

clean:
	rm *.hi *.o *.dyn_hi *.dyn_o $(PROJ).json
