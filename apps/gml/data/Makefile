
ifdef WINDIR
EXE=.exe
else
EXE=
endif

TARGETDIR = ../build

GML_O   = $(TARGETDIR)/gml_o.exe
GML_OR  = $(TARGETDIR)/gml_or.exe
GML_I   = $(TARGETDIR)/gml_i.exe
GML_IP  = $(TARGETDIR)/gml_ip.exe
GML_IR  = $(TARGETDIR)/gml_ir.exe
GML_IRP = $(TARGETDIR)/gml_irp.exe

PLCLUB = $(TARGETDIR)/raytrace$(EXE)
PLCLUB_P = $(TARGETDIR)/raytrace_p$(EXE)

.PHONY: all_ref

all_ref:
	make chess_ref.ppm
	make dice_ref.ppm
	make fractal_ref.ppm
	make go_ref.ppm
	make kal_ref.ppm
	make mandel_ref.ppm
	make snowgoon_ref.ppm
	make texture_ref.ppm
	make tree_ref.ppm

%_o.ppm: %.gml $(GML_O)
	time $(GML_O) $<
	mv $(<:.gml=.ppm) $@

%_or.ppm: %.gml $(GML_OR)
	time $(GML_OR) $<
	mv $(<:.gml=.ppm) $@

%_i.ppm: %.gml $(GML_I)
	time $(GML_I) $< < /dev/null
	mv $(<:.gml=.ppm) $@

%_ip.ppm: %.gml $(GML_IP)
	time $(GML_IP) $<
	mv fmon.data $(@:.ppm=.data)
	mv $(<:.gml=.ppm) $@

%_ir.ppm: %.gml $(GML_IR)
	time $(GML_IR) $< < /dev/null
	mv $(<:.gml=.ppm) $@

%_irp.ppm: %.gml $(GML_IRP)
	time $(GML_IRP) $<
	mv fmon.data $(@:.ppm=.data)
	mv $(<:.gml=.ppm) $@

%_ref.ppm: %.gml $(PLCLUB)
	time $(PLCLUB) $< < /dev/null
	mv $(<:.gml=.ppm) $@

%_pref.ppm: %.gml $(PLCLUB_P)
	time $(PLCLUB_P) $< < /dev/null
	mv $(<:.gml=.ppm) $@

%_diff_i.ppm: %_ref.ppm %_i.ppm
	../plclub/src/difference$(EXE) $< $(<:_ref.ppm=_i.ppm) $@

%_diff_ip.ppm: %_ref.ppm %_ip.ppm
	../plclub/src/difference$(EXE) $< $(<:_ref.ppm=_ip.ppm) $@

%_diff_o.ppm: %_ref.ppm %_o.ppm
	../plclub/src/difference.exe $< $(<:_ref.ppm=_o.ppm) $@

clean:
	rm -f *.ppm *~