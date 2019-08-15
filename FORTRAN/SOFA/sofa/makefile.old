#-----------------------------------------------------------------------
#
# Description:  make file for the SOFA library.
#
# Usage:
#
#    To make the library, type:
#
#      make
#
#    To delete all object files, type:
#
#      make clean
#
# This revision:  2003 March 17
#
# Copright (C) 2003 IAU SOFA Review Board.  All rights reserved.
#-----------------------------------------------------------------------

# This suite of functions is only compilable by ANSI Fortran compilers -
# give the name of your preferred Fortran compiler and compilation flags
# here.

FC = f77
FF = -c -O

#----YOU SHOULDN'T HAVE TO MODIFY ANYTHING BELOW THIS LINE---------

# Name the SOFA library in its source location.

SOFA_LIB = libsofa.a

# The list of SOFA library object files.

SOFA_OBS = iau_A2AF.o \
           iau_A2TF.o \
           iau_ANP.o \
           iau_ANPM.o \
           iau_BI00.o \
           iau_BP00.o \
           iau_BPN2XY.o \
           iau_C2I00A.o \
           iau_C2I00B.o \
           iau_C2IBPN.o \
           iau_C2IXY.o \
           iau_C2IXYS.o \
           iau_C2S.o \
           iau_C2T00A.o \
           iau_C2T00B.o \
           iau_C2TCEO.o \
           iau_C2TEQX.o \
           iau_C2TPE.o \
           iau_C2TXY.o \
           iau_CAL2JD.o \
           iau_CP.o \
           iau_CPV.o \
           iau_CR.o \
           iau_D2TF.o \
           iau_DAT.o \
           iau_DTDB.o \
           iau_EE00.o \
           iau_EE00A.o \
           iau_EE00B.o \
           iau_EECT00.o \
           iau_EPB.o \
           iau_EPB2JD.o \
           iau_EPJ.o \
           iau_EPJ2JD.o \
           iau_EPV00.o \
           iau_EQEQ94.o \
           iau_ERA00.o \
           iau_FK52H.o \
           iau_FK5HIP.o \
           iau_FK5HZ.o \
           iau_GMST00.o \
           iau_GMST82.o \
           iau_GST00A.o \
           iau_GST00B.o \
           iau_GST94.o \
           iau_H2FK5.o \
           iau_HFK5Z.o \
           iau_IR.o \
           iau_JD2CAL.o \
           iau_JDCALF.o \
           iau_NUM00A.o \
           iau_NUM00B.o \
           iau_NUMAT.o \
           iau_NUT00A.o \
           iau_NUT00B.o \
           iau_NUT80.o \
           iau_NUTM80.o \
           iau_OBL80.o \
           iau_P2PV.o \
           iau_P2S.o \
           iau_PAP.o \
           iau_PAS.o \
           iau_PDP.o \
           iau_PLAN94.o \
           iau_PM.o \
           iau_PMAT00.o \
           iau_PMAT76.o \
           iau_PMP.o \
           iau_PN.o \
           iau_PN00.o \
           iau_PN00A.o \
           iau_PN00B.o \
           iau_PNM00A.o \
           iau_PNM00B.o \
           iau_PNM80.o \
           iau_POM00.o \
           iau_PPP.o \
           iau_PPSP.o \
           iau_PR00.o \
           iau_PREC76.o \
           iau_PV2P.o \
           iau_PV2S.o \
           iau_PVDPV.o \
           iau_PVM.o \
           iau_PVMPV.o \
           iau_PVPPV.o \
           iau_PVSTAR.o \
           iau_PVU.o \
           iau_PVUP.o \
           iau_PVXPV.o \
           iau_PXP.o \
           iau_RM2V.o \
           iau_RV2M.o \
           iau_RX.o \
           iau_RXP.o \
           iau_RXPV.o \
           iau_RXR.o \
           iau_RY.o \
           iau_RZ.o \
           iau_S00.o \
           iau_S00A.o \
           iau_S00B.o \
           iau_S2C.o \
           iau_S2P.o \
           iau_S2PV.o \
           iau_S2XPV.o \
           iau_SEPP.o \
           iau_SEPS.o \
           iau_SP00.o \
           iau_STARPM.o \
           iau_STARPV.o \
           iau_SXP.o \
           iau_SXPV.o \
           iau_TR.o \
           iau_TRXP.o \
           iau_TRXPV.o \
           iau_XYS00A.o \
           iau_XYS00B.o \
           iau_ZP.o \
           iau_ZPV.o \
           iau_ZR.o

#-----------------------------------------------------------------------

default: $(SOFA_LIB)

# Make and install the library.

$(SOFA_LIB): $(SOFA_OBS)
	ar ru $(SOFA_LIB) $?

# Clean up.

clean:
	rm -f $(SOFA_OBS)

#-----------------------------------------------------------------------
# The list of object file dependencies.

iau_A2AF.o : a2af.for
	$(FC) $(FF) -o $@ a2af.for
iau_A2TF.o : a2tf.for
	$(FC) $(FF) -o $@ a2tf.for
iau_ANP.o : anp.for
	$(FC) $(FF) -o $@ anp.for
iau_ANPM.o : anpm.for
	$(FC) $(FF) -o $@ anpm.for
iau_BI00.o : bi00.for
	$(FC) $(FF) -o $@ bi00.for
iau_BP00.o : bp00.for
	$(FC) $(FF) -o $@ bp00.for
iau_BPN2XY.o : bpn2xy.for
	$(FC) $(FF) -o $@ bpn2xy.for
iau_C2I00A.o : c2i00a.for
	$(FC) $(FF) -o $@ c2i00a.for
iau_C2I00B.o : c2i00b.for
	$(FC) $(FF) -o $@ c2i00b.for
iau_C2IBPN.o : c2ibpn.for
	$(FC) $(FF) -o $@ c2ibpn.for
iau_C2IXY.o : c2ixy.for
	$(FC) $(FF) -o $@ c2ixy.for
iau_C2IXYS.o : c2ixys.for
	$(FC) $(FF) -o $@ c2ixys.for
iau_C2S.o : c2s.for
	$(FC) $(FF) -o $@ c2s.for
iau_C2T00A.o : c2t00a.for
	$(FC) $(FF) -o $@ c2t00a.for
iau_C2T00B.o : c2t00b.for
	$(FC) $(FF) -o $@ c2t00b.for
iau_C2TCEO.o : c2tceo.for
	$(FC) $(FF) -o $@ c2tceo.for
iau_C2TEQX.o : c2teqx.for
	$(FC) $(FF) -o $@ c2teqx.for
iau_C2TPE.o : c2tpe.for
	$(FC) $(FF) -o $@ c2tpe.for
iau_C2TXY.o : c2txy.for
	$(FC) $(FF) -o $@ c2txy.for
iau_CAL2JD.o : cal2jd.for
	$(FC) $(FF) -o $@ cal2jd.for
iau_CP.o : cp.for
	$(FC) $(FF) -o $@ cp.for
iau_CPV.o : cpv.for
	$(FC) $(FF) -o $@ cpv.for
iau_CR.o : cr.for
	$(FC) $(FF) -o $@ cr.for
iau_D2TF.o : d2tf.for
	$(FC) $(FF) -o $@ d2tf.for
iau_DAT.o : dat.for
	$(FC) $(FF) -o $@ dat.for
iau_DTDB.o : dtdb.for
	$(FC) $(FF) -o $@ dtdb.for
iau_EE00.o : ee00.for
	$(FC) $(FF) -o $@ ee00.for
iau_EE00A.o : ee00a.for
	$(FC) $(FF) -o $@ ee00a.for
iau_EE00B.o : ee00b.for
	$(FC) $(FF) -o $@ ee00b.for
iau_EECT00.o : eect00.for
	$(FC) $(FF) -o $@ eect00.for
iau_EPB.o : epb.for
	$(FC) $(FF) -o $@ epb.for
iau_EPB2JD.o : epb2jd.for
	$(FC) $(FF) -o $@ epb2jd.for
iau_EPJ.o : epj.for
	$(FC) $(FF) -o $@ epj.for
iau_EPJ2JD.o : epj2jd.for
	$(FC) $(FF) -o $@ epj2jd.for
iau_EPV00.o : epv00.for
	$(FC) $(FF) -o $@ epv00.for
iau_EQEQ94.o : eqeq94.for
	$(FC) $(FF) -o $@ eqeq94.for
iau_ERA00.o : era00.for
	$(FC) $(FF) -o $@ era00.for
iau_FK52H.o : fk52h.for
	$(FC) $(FF) -o $@ fk52h.for
iau_FK5HIP.o : fk5hip.for
	$(FC) $(FF) -o $@ fk5hip.for
iau_FK5HZ.o : fk5hz.for
	$(FC) $(FF) -o $@ fk5hz.for
iau_GMST00.o : gmst00.for
	$(FC) $(FF) -o $@ gmst00.for
iau_GMST82.o : gmst82.for
	$(FC) $(FF) -o $@ gmst82.for
iau_GST00A.o : gst00a.for
	$(FC) $(FF) -o $@ gst00a.for
iau_GST00B.o : gst00b.for
	$(FC) $(FF) -o $@ gst00b.for
iau_GST94.o : gst94.for
	$(FC) $(FF) -o $@ gst94.for
iau_H2FK5.o : h2fk5.for
	$(FC) $(FF) -o $@ h2fk5.for
iau_HFK5Z.o : hfk5z.for
	$(FC) $(FF) -o $@ hfk5z.for
iau_IR.o : ir.for
	$(FC) $(FF) -o $@ ir.for
iau_JD2CAL.o : jd2cal.for
	$(FC) $(FF) -o $@ jd2cal.for
iau_JDCALF.o : jdcalf.for
	$(FC) $(FF) -o $@ jdcalf.for
iau_NUM00A.o : num00a.for
	$(FC) $(FF) -o $@ num00a.for
iau_NUM00B.o : num00b.for
	$(FC) $(FF) -o $@ num00b.for
iau_NUMAT.o : numat.for
	$(FC) $(FF) -o $@ numat.for
iau_NUT00A.o : nut00a.for
	$(FC) $(FF) -o $@ nut00a.for
iau_NUT00B.o : nut00b.for
	$(FC) $(FF) -o $@ nut00b.for
iau_NUT80.o : nut80.for
	$(FC) $(FF) -o $@ nut80.for
iau_NUTM80.o : nutm80.for
	$(FC) $(FF) -o $@ nutm80.for
iau_OBL80.o : obl80.for
	$(FC) $(FF) -o $@ obl80.for
iau_P2PV.o : p2pv.for
	$(FC) $(FF) -o $@ p2pv.for
iau_P2S.o : p2s.for
	$(FC) $(FF) -o $@ p2s.for
iau_PAP.o : pap.for
	$(FC) $(FF) -o $@ pap.for
iau_PAS.o : pas.for
	$(FC) $(FF) -o $@ pas.for
iau_PDP.o : pdp.for
	$(FC) $(FF) -o $@ pdp.for
iau_PLAN94.o : plan94.for
	$(FC) $(FF) -o $@ plan94.for
iau_PM.o : pm.for
	$(FC) $(FF) -o $@ pm.for
iau_PMAT00.o : pmat00.for
	$(FC) $(FF) -o $@ pmat00.for
iau_PMAT76.o : pmat76.for
	$(FC) $(FF) -o $@ pmat76.for
iau_PMP.o : pmp.for
	$(FC) $(FF) -o $@ pmp.for
iau_PN.o : pn.for
	$(FC) $(FF) -o $@ pn.for
iau_PN00.o : pn00.for
	$(FC) $(FF) -o $@ pn00.for
iau_PN00A.o : pn00a.for
	$(FC) $(FF) -o $@ pn00a.for
iau_PN00B.o : pn00b.for
	$(FC) $(FF) -o $@ pn00b.for
iau_PNM00A.o : pnm00a.for
	$(FC) $(FF) -o $@ pnm00a.for
iau_PNM00B.o : pnm00b.for
	$(FC) $(FF) -o $@ pnm00b.for
iau_PNM80.o : pnm80.for
	$(FC) $(FF) -o $@ pnm80.for
iau_POM00.o : pom00.for
	$(FC) $(FF) -o $@ pom00.for
iau_PPP.o : ppp.for
	$(FC) $(FF) -o $@ ppp.for
iau_PPSP.o : ppsp.for
	$(FC) $(FF) -o $@ ppsp.for
iau_PR00.o : pr00.for
	$(FC) $(FF) -o $@ pr00.for
iau_PREC76.o : prec76.for
	$(FC) $(FF) -o $@ prec76.for
iau_PV2P.o : pv2p.for
	$(FC) $(FF) -o $@ pv2p.for
iau_PV2S.o : pv2s.for
	$(FC) $(FF) -o $@ pv2s.for
iau_PVDPV.o : pvdpv.for
	$(FC) $(FF) -o $@ pvdpv.for
iau_PVM.o : pvm.for
	$(FC) $(FF) -o $@ pvm.for
iau_PVMPV.o : pvmpv.for
	$(FC) $(FF) -o $@ pvmpv.for
iau_PVPPV.o : pvppv.for
	$(FC) $(FF) -o $@ pvppv.for
iau_PVSTAR.o : pvstar.for
	$(FC) $(FF) -o $@ pvstar.for
iau_PVU.o : pvu.for
	$(FC) $(FF) -o $@ pvu.for
iau_PVUP.o : pvup.for
	$(FC) $(FF) -o $@ pvup.for
iau_PVXPV.o : pvxpv.for
	$(FC) $(FF) -o $@ pvxpv.for
iau_PXP.o : pxp.for
	$(FC) $(FF) -o $@ pxp.for
iau_RM2V.o : rm2v.for
	$(FC) $(FF) -o $@ rm2v.for
iau_RV2M.o : rv2m.for
	$(FC) $(FF) -o $@ rv2m.for
iau_RX.o : rx.for
	$(FC) $(FF) -o $@ rx.for
iau_RXP.o : rxp.for
	$(FC) $(FF) -o $@ rxp.for
iau_RXPV.o : rxpv.for
	$(FC) $(FF) -o $@ rxpv.for
iau_RXR.o : rxr.for
	$(FC) $(FF) -o $@ rxr.for
iau_RY.o : ry.for
	$(FC) $(FF) -o $@ ry.for
iau_RZ.o : rz.for
	$(FC) $(FF) -o $@ rz.for
iau_S00.o : s00.for
	$(FC) $(FF) -o $@ s00.for
iau_S00A.o : s00a.for
	$(FC) $(FF) -o $@ s00a.for
iau_S00B.o : s00b.for
	$(FC) $(FF) -o $@ s00b.for
iau_S2C.o : s2c.for
	$(FC) $(FF) -o $@ s2c.for
iau_S2P.o : s2p.for
	$(FC) $(FF) -o $@ s2p.for
iau_S2PV.o : s2pv.for
	$(FC) $(FF) -o $@ s2pv.for
iau_S2XPV.o : s2xpv.for
	$(FC) $(FF) -o $@ s2xpv.for
iau_SEPP.o : sepp.for
	$(FC) $(FF) -o $@ sepp.for
iau_SEPS.o : seps.for
	$(FC) $(FF) -o $@ seps.for
iau_SP00.o : sp00.for
	$(FC) $(FF) -o $@ sp00.for
iau_STARPM.o : starpm.for
	$(FC) $(FF) -o $@ starpm.for
iau_STARPV.o : starpv.for
	$(FC) $(FF) -o $@ starpv.for
iau_SXP.o : sxp.for
	$(FC) $(FF) -o $@ sxp.for
iau_SXPV.o : sxpv.for
	$(FC) $(FF) -o $@ sxpv.for
iau_TR.o : tr.for
	$(FC) $(FF) -o $@ tr.for
iau_TRXP.o : trxp.for
	$(FC) $(FF) -o $@ trxp.for
iau_TRXPV.o : trxpv.for
	$(FC) $(FF) -o $@ trxpv.for
iau_XYS00A.o : xys00a.for
	$(FC) $(FF) -o $@ xys00a.for
iau_XYS00B.o : xys00b.for
	$(FC) $(FF) -o $@ xys00b.for
iau_ZP.o : zp.for
	$(FC) $(FF) -o $@ zp.for
iau_ZPV.o : zpv.for
	$(FC) $(FF) -o $@ zpv.for
iau_ZR.o : zr.for
	$(FC) $(FF) -o $@ zr.for

#-----------------------------------------------------------------------
