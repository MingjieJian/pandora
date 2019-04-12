      subroutine ALEXIUS
     $(ROTN,FUND,OVER,SECN)
C
C     Rudolf Loeser, 1994 Aug 15
C---- Sets up debug checksums for CO-lines opacity data.
C     !DASH
      save
C     !DASH
      logical FUND, OVER, ROTN, SECN
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
      external CHECKER, HI, BYE
C
      call HI ('ALEXIUS')
C     !BEG
      call CHECKER   (EXEN(1,1,1),1,(JCOMAX+1)*KCOMAX,'CO-EXEN/12')
      call CHECKER   (EXEN(1,1,2),1,(JCOMAX+1)*KCOMAX,'CO-EXEN/13')
      if(ROTN) then
        call CHECKER (WNROT(1,1,1),1,JCOLMR*KCOLMR,'CO-WNROT/12')
        call CHECKER (WNROT(1,1,2),1,JCOLMR*KCOLMR,'CO-WNROT/13')
        call CHECKER ( FROT(1,1,1),1,JCOLMR*KCOLMR, 'CO-FROT/12')
        call CHECKER ( FROT(1,1,2),1,JCOLMR*KCOLMR, 'CO-FROT/13')
      end if
      if(FUND) then
        call CHECKER (WNUPF(1,1,1),1,JCOLMF*KCOLMF,'CO-WNUPF/12')
        call CHECKER (WNUPF(1,1,2),1,JCOLMF*KCOLMF,'CO-WNUPF/13')
        call CHECKER (WNDNF(1,1,1),1,JCOLMF*KCOLMF,'CO-WNDNF/12')
        call CHECKER (WNDNF(1,1,2),1,JCOLMF*KCOLMF,'CO-WNDNF/13')
        call CHECKER ( FUPF(1,1,1),1,JCOLMF*KCOLMF, 'CO-FUPF/12')
        call CHECKER ( FUPF(1,1,2),1,JCOLMF*KCOLMF, 'CO-FUPF/13')
        call CHECKER ( FDNF(1,1,1),1,JCOLMF*KCOLMF, 'CO-FDNF/12')
        call CHECKER ( FDNF(1,1,2),1,JCOLMF*KCOLMF, 'CO-FDNF/13')
      end if
      if(OVER) then
        call CHECKER (WNUPO(1,1,1),1,JCOLMO*KCOLMO,'CO-WNUPO/12')
        call CHECKER (WNUPO(1,1,2),1,JCOLMO*KCOLMO,'CO-WNUPO/13')
        call CHECKER (WNDNO(1,1,1),1,JCOLMO*KCOLMO,'CO-WNDNO/12')
        call CHECKER (WNDNO(1,1,2),1,JCOLMO*KCOLMO,'CO-WNDNO/13')
        call CHECKER ( FUPO(1,1,1),1,JCOLMO*KCOLMO, 'CO-FUPO/12')
        call CHECKER ( FUPO(1,1,2),1,JCOLMO*KCOLMO, 'CO-FUPO/13')
        call CHECKER ( FDNO(1,1,1),1,JCOLMO*KCOLMO, 'CO-FDNO/12')
        call CHECKER ( FDNO(1,1,2),1,JCOLMO*KCOLMO, 'CO-FDNO/13')
      end if
      if(SECN) then
        call CHECKER (WNUPS(1,1,1),1,JCOLMS*KCOLMS,'CO-WNUPS/12')
        call CHECKER (WNUPS(1,1,2),1,JCOLMS*KCOLMS,'CO-WNUPS/13')
        call CHECKER (WNDNS(1,1,1),1,JCOLMS*KCOLMS,'CO-WNDNS/12')
        call CHECKER (WNDNS(1,1,2),1,JCOLMS*KCOLMS,'CO-WNDNS/13')
        call CHECKER ( FUPS(1,1,1),1,JCOLMS*KCOLMS, 'CO-FUPS/12')
        call CHECKER ( FUPS(1,1,2),1,JCOLMS*KCOLMS, 'CO-FUPS/13')
        call CHECKER ( FDNS(1,1,1),1,JCOLMS*KCOLMS, 'CO-FDNS/12')
        call CHECKER ( FDNS(1,1,2),1,JCOLMS*KCOLMS, 'CO-FDNS/13')
      end if
C     !END
      call BYE ('ALEXIUS')
C
      return
      end
