clients: jpl nsidc

jpl:
	L4.pl -install $(OPTIONS) -target $@

nsidc:
	L4.pl -install $(OPTIONS) -target $@

all: fp fpp rproc test

fp:
	L4.pl -st SMAP -sn OPS $(OPTIONS) -target $@
	L4.pl -st SPL4SM -sn SPL4SM_Vv4010 $(OPTIONS) -target $@
	L4.pl -st SPL4SM -sn SPL4SM_Vv4030 $(OPTIONS) -target $@
	L4.pl -st SPL4C  -sn SPL4C_Vv4020 $(OPTIONS) -target $@

fpp:
	L4.pl -st SMAP -sn RPROC $(OPTIONS) -target $@

rproc:
	L4.pl -st SMAP -sn RPROC $(OPTIONS) -target $@
	L4.pl -st SPL4SM -sn SPL4SM_OL4001 $(OPTIONS) -target $@
	L4.pl -st SPL4SM -sn SPL4SM_OL4002 $(OPTIONS) -target $@

null:
	L4.pl -st SMAP -sn OPS $(OPTIONS) -target $@
	L4.pl -st SPL4SM -sn SPL4SM_V10000 $(OPTIONS) -target $@
	L4.pl -st SPL4C -sn SPL4C_V10000 $(OPTIONS) -target $@

test:
	L4.pl -st SMAP -sn OASIS $(OPTIONS) -target $@

oasis:
	L4.pl -st SMAP -sn OASIS $(OPTIONS) -target $@

nature:
	L4.pl -st SPL4NR -sn SMAP_Nature_v03 $(OPTIONS) -target $@

report_granule:
	L4.pl -report granule -st SPL4 -sn fp -task export/jpl $(OPTIONS) -target $@
	L4.pl -report granule -st SPL4 -sn fp -task export/nsidc $(OPTIONS) -target $@
	L4.pl -report granule -st SPL4 -sn fpp -task export/jpl $(OPTIONS) -target $@
	L4.pl -report granule -st SPL4 -sn fpp -task export/nsidc $(OPTIONS) -target $@
	L4.pl -report granule -st SPL4 -sn rproc -task export/jpl $(OPTIONS) -target $@
	L4.pl -report granule -st SPL4 -sn rproc -task export/nsidc $(OPTIONS) -target $@

report_summary:
	L4.pl -report summary -st SPL4 -sn fp -task export/jpl $(OPTIONS) -target $@
	L4.pl -report summary -st SPL4 -sn fpp -task export/jpl $(OPTIONS) -target $@
	L4.pl -report summary -st SPL4 -sn rproc -task export/jpl $(OPTIONS) -target $@

respond:
	L4.pl -respond -st SMAP -sn OPS -task acquire $(OPTIONS) -target $@
	L4.pl -respond -st SMAP -sn RPROC -task acquire $(OPTIONS) -target $@
