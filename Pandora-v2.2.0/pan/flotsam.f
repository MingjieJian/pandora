      subroutine FLOTSAM
     $(LU)
C
C     Rudolf Loeser, 1992 Sep 14
C---- Prints CO lines data, for EYAK.
C     (This is version 5 of FLOTSAM.)
C     !DASH
      save
C     !DASH
      integer LU
C     !COM
C---- LOOM        as of 1994 Dec 16
      integer     JCOLMF,KCOLMF,JCOLMO,KCOLMO,JCOLMS,KCOLMS,
     $            JCOLMR,KCOLMR,JCOMAX,KCOMAX
      parameter   (JCOLMF=111, KCOLMF=20)
      parameter   (JCOLMO=111, KCOLMO=13)
      parameter   (JCOLMS=111, KCOLMS=12)
      parameter   (JCOLMR=53,  KCOLMR=21)
      parameter   (JCOMAX=max(JCOLMF,JCOLMO,JCOLMS,JCOLMR))
      parameter   (KCOMAX=max(KCOLMF,KCOLMO,KCOLMS,KCOLMR))
C     (Be sure to recompile all users of LOOM when changing any
C     of the above parameter values!)
      real*8      WNUPF,WNDNF,WNUPO,WNDNO,WNUPS,WNDNS,WNROT,
     $            FUPF ,FDNF ,FUPO ,FDNO ,FUPS ,FDNS ,FROT ,
     $            EXEN,WVCOLO,WVCOHI,RC1213
      integer     JCOFUN,KCOFUN,JCOOVR,KCOOVR,JCOSEC,KCOSEC,
     $            JCOROT,KCOROT,ISOSLCT,METHCOW,METHCOF
      logical     FUNDCO,OVERCO,SECNCO,ROTACO
C
      dimension   WNUPF(JCOLMF,KCOLMF,2), WNDNF(JCOLMF,KCOLMF,2),
     $            WNUPO(JCOLMO,KCOLMO,2), WNDNO(JCOLMO,KCOLMO,2),
     $            WNUPS(JCOLMS,KCOLMS,2), WNDNS(JCOLMS,KCOLMS,2),
     $            WNROT(JCOLMR,KCOLMR,2),
     $            FUPF(JCOLMF,KCOLMF,2),  FDNF(JCOLMF,KCOLMF,2),
     $            FUPO(JCOLMO,KCOLMO,2),  FDNO(JCOLMO,KCOLMO,2),
     $            FUPS(JCOLMS,KCOLMS,2),  FDNS(JCOLMS,KCOLMS,2),
     $            FROT(JCOLMR,KCOLMR,2),
     $            EXEN(JCOMAX+1,KCOMAX,2)
      dimension   FUNDCO(2),OVERCO(2),SECNCO(2),ROTACO(2),
     $            JCOFUN(2),KCOFUN(2),JCOOVR(2),KCOOVR(2),
     $            JCOSEC(2),KCOSEC(2),JCOROT(2),KCOROT(2)
C
C     Carbon Monoxide lines:  wavenumbers and oscillator strengths.
C
C     WNROT, etc., are for rotational lines     : j'-j=+1, v'-v=0;
C     WNUPF, etc., are for fundamental lines    : j'-j=+1, v'-v=1;
C     WNDNF, etc., are for fundamental lines    : j'-j=-1, v'-v=1;
C     WNUPO, etc., are for first overtone lines : j'-j=+1, v'-v=2;
C     WNDNO, etc., are for first overtone lines : j'-j=-1, v'-v=2;
C     WNUPS, etc., are for second overtone lines: j'-j=+1, v'-v=3;
C     WNDNS, etc., are for second overtone lines: j'-j=-1, v'-v=3.
C
C     This is how the array indices relate to the quantum numbers:
C
C     1. index J corresponds to quantum number j -
C                in WNROT, etc., j = J-1, j' = J
C                in WNUPF, etc., j = J-1, j' = J
C                in WNDNF, etc., j = J  , j' = J-1
C                in WNUPO, etc., j = J-1, j' = J
C                in WNDNO, etc., j = J  , j' = J-1
C                in WNUPS, etc., j = J-1, j' = J
C                in WNDNS, etc., j = J  , j' = J-1
C
C     2. index K corresponds to quantum number v (=nu) -
C                in WNROT, etc.,                 v = K-1, v' = K-1
C                in WNUPF, etc. and WNDNF, etc., v = K-1, v' = K
C                in WNUPO, etc. and WNDNO, etc., v = K-1, v' = K+1
C                in WNUPS, etc. and WNDNS, etc., v = K-1, v' = K+2
C
C     3. index L identifies the Carbon isotope -
C                L=1 for Carbon-12, L=2 for Carbon-13.
C
C     Wavenumbers and energies from:
C        Farrenq, R., Guelachvili, G., Sauval, A.J., Grevesse, N.,
C        and Farmer, C.B. 1991,
C        J.Molec.Spectrosc., 149, 375
C       or
C        Coxon, J.A., and Hajigeorgiou, P.G. 1992,
C        Can.J.Phys., 70, 40-54
C       or
C        Goorvitch, 1994, Ap.J.Suppl.
C     f-values from:
C        Chackerian, C., Jr, and Tipping, R. H. 1983
C        J.Molec.Spectros., 99, 431
C       or
C        "improved" Chackerian
C       or
C        Goorvitch, 1994, Ap.J.Suppl.
C
      common      /LOOM1 / EXEN
      common      /LOOM2 / WNROT,FROT
      common      /LOOM3 / WNUPF,FUPF,WNDNF,FDNF
      common      /LOOM4 / WNUPO,FUPO,WNDNO,FDNO
      common      /LOOM5 / WNUPS,FUPS,WNDNS,FDNS
      common      /LOOM6 / JCOFUN,KCOFUN,JCOOVR,KCOOVR,JCOSEC,KCOSEC,
     $                     JCOROT,KCOROT
      common      /LOOM7 / WVCOLO,WVCOHI
      common      /LOOM8 / FUNDCO,OVERCO,SECNCO,ROTACO
C
C     (These parameters are computed by subroutines WEAVE and FOCA.)
C
      common      /LOOM9 / RC1213
      common      /LOOM10/ ISOSLCT,METHCOW,METHCOF
C
C     RC1213  is the C(12)/C(13) isotopic abundance ratio.
C     ISOSLCT controls which isotopes are included in this calculation:
C     ISOSLCT = 1 means: C(12) only;
C     ISOSLCT = 2 means: C(13) only; and
C     ISOSLCT = 3 means: both C(12) and C(13).
C
C     METHCOF = 1 means: use f-values from Chackerian & Tipping (1983);
C     METHCOF = 2 means: use f-values from "new" Chackerian data;
C     METHCOF = 3 means: use f-values from Goorvitch (1994).
C
C     METHCOW = 1 means: use energies from Farrenq et al.;
C     METHCOW = 2 means: use energies from Coxon & Hajigeorgiou;
C     METHCOW = 3 means: use wave numbers & energies from
C                                                    Goorvitch (1994).
C     .
C     !DASH
      external LINER, HI, BYE
C
      call HI ('FLOTSAM')
C     !BEG
      if(LU.gt.0) then
        call LINER (1,LU)
        write (LU,100) 12,JCOROT(1),KCOROT(1),JCOFUN(1),KCOFUN(1),
     $                    JCOOVR(1),KCOOVR(1),JCOSEC(1),KCOSEC(1)
        write (LU,100) 13,JCOROT(2),KCOROT(2),JCOFUN(2),KCOFUN(2),
     $                    JCOOVR(2),KCOOVR(2),JCOSEC(2),KCOSEC(2)
  100   format(' ','C(',I2,') data tables limits - ',
     $             'JROT:', I4,3X,'KROT:', I3,3X,
     $             'JFUND:',I4,3X,'KFUND:',I3,3X,
     $             'JOVER:',I4,3X,'KOVER:',I3,3X,
     $             'JSECN:',I4,3X,'KSECN:',I3)
        call LINER (1,LU)
        write (LU,101) RC1213
  101   format(' ','Isotopic Carbon abundance ratio C(12)/C(13): ',
     $             'RC1213 =',1PE11.4)
        call LINER (1,LU)
        write (LU,102) ISOSLCT
  102   format(' ','Switch controlling which isotope(s) to include ',
     $             'in this run: ISOSLCT =',I2/
     $         ' ','ISOSLCT = 1 means: C(12) only; ',
     $             'ISOSLCT = 2 means: C(13) only; ',
     $             'ISOSLCT = 3 means: both C(12) and C(13)')
        call LINER (1,LU)
        write (LU,103) METHCOW,METHCOF
  103   format(' ','METHCOW = 1 means: compute energies from ',
     $             'Farrenq et al. (as amended 5/93)'/
     $         ' ','METHCOW = 2 means: compute energies from ',
     $             'Coxon & Hajigeorgiou'/
     $         ' ','METHCOW = 3 means: use values of wavenumbers ',
     $             'and energies tabulated by Goorvitch (1994)'/
     $         ' ','METHCOW =',I2,' in this run.'/
     $         ' ','METHCOF = 1 means: compute f-values from ',
     $             'Chackerian & Tipping (1983)'/
     $         ' ','METHCOF = 2 means: compute f-values from ',
     $             '"new" Chackerian data (1993), supplemented by ',
     $             'Chackerian & Tipping (1983)'/
     $         ' ','METHCOF = 3 means: use f-values tabulated by ',
     $             'Goorvitch (1994)'/
     $         ' ','METHCOF =',I2,' in this run.')
      end if
C     !END
      call BYE ('FLOTSAM')
C
      return
      end
