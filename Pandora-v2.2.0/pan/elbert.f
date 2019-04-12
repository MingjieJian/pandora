      subroutine ELBERT
C
C     Rudolf Loeser, 1992 Sep 22
C---- Computes fundamental CO lines opacity contribution.
C     !DASH
      save
C     !DASH
      real*8 ONE, XCOMX
      logical YES, YES1, YESK
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
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ( 52),XCOMX)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C
C---- MOLONGA     as of 2003 Dec 02
      integer     I,JUD,L,KOD,M
      real*8      T,H,C1,C2,C3,C4,AX,SHFT,WLP,WLM,ABISO,ET,SUM
      logical     DPL,DMP,STT
C
      dimension   C1(2),C3(2),C4(2),WLP(2),WLM(2)
      common      /MOLONG1/ T,H,C1,C2,C3,C4,AX,SHFT,WLP,WLM,ABISO,
     $                      ET,SUM
      common      /MOLONG2/ I,JUD,L,KOD,M
      common      /MOLONG3/ DPL,DMP,STT
C     Intermediates for CO-opacity calculation.
C     .
C     !DASH
C     !EJECT
      external  ARATUS, FLOUR, HI, BYE
      intrinsic abs
C
      call HI ('ELBERT')
C     !BEG
      KOD = 1
      do 103 M = 1,2
        if(FUNDCO(M)) then
          call ARATUS        (M, RC1213, ABISO)
          do 102 I = 1,JCOFUN(M)
C
C----       UP transitions: j -> j+1
            JUD  = +1
            YES1 = WLM(M).le.WNUPF(I,1,M)
            YESK = WLP(M).ge.WNUPF(I,KCOFUN(M),M)
            YES  = YES1.and.YESK
            if(YES) then
              do 100 L = 1,KCOFUN(M)
                AX = abs(C1(M)*((C2*WNUPF(I,L,M)-ONE)+SHFT))
                if(AX.lt.XCOMX) then
                  call FLOUR (WNUPF(I,L,M), FUPF(I,L,M), EXEN(I,L,M))
                end if
  100         continue
            end if
C
C----       DOWN transitions: j -> j-1
            JUD  = -1
            YES1 = WLM(M).le.WNDNF(I,1,M)
            YESK = WLP(M).ge.WNDNF(I,KCOFUN(M),M)
            YES  = YES1.and.YESK
            if(YES) then
              do 101 L = 1,KCOFUN(M)
                AX = abs(C1(M)*((C2*WNDNF(I,L,M)-ONE)+SHFT))
                if(AX.lt.XCOMX) then
                  call FLOUR (WNDNF(I,L,M), FDNF(I,L,M), EXEN(I+1,L,M))
                end if
  101         continue
            end if
C
  102     continue
        end if
C
  103 continue
C     !END
      call BYE ('ELBERT')
C
      return
      end
