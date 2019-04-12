      subroutine CHILKAT
     $(QNAME)
C
C     Rudolf Loeser, 1992 Sep 14
C---- Reads CO lines data tables control indices.
C     (This is version 3 of CHILKAT.)
C     !DASH
      save
C     !DASH
      real*8 dummy
      integer I, KERR, KIND, LOOK, LUEO, MODE, NIND, jummy
      character QIND*8, QNAME*8, QNOME*8, qummy*8
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
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external  MACE, KIWI, LOOKUC, MESHED, CHLOE, ABORT, UNMIX, CARMEN,
     $          HI, BYE
      intrinsic min,max
C
      dimension QIND(13)
C
      data NIND /13/
      data QIND /'JFUND', 'KFUND', 'JOVER', 'KOVER', 'JROT', 'KROT',
     $           'JSECN', 'KSECN', 'ISOSLCT', 'METHCOF', 'METHCOW',
     $           'RC1213', ')'/
C
      call HI ('CHILKAT')
C     !BEG
      KERR = 0
      call MACE
  100 continue
        call KIWI   (MODE, dummy, jummy, QNOME, jummy)
        if(MODE.ne.2) goto 201
C
        call UNMIX  (QNOME)
        call LOOKUC (QIND, NIND, QNOME, KIND, LOOK)
        if(LOOK.ne.1) goto 202
        if(KIND.eq.NIND) goto 199
C     !EJECT
        if(KIND.le.(NIND-2)) then
          call KIWI (MODE, dummy, I, qummy, jummy)
          if(MODE.ne.3) goto 203
          goto (101, 102, 103, 104, 105, 106, 107, 108, 109, 110,
     $          111 ), KIND
C
  101     continue
            JCOFUN(1) = min(JCOLMF,I)
            JCOFUN(2) = min(JCOLMF,I)
            goto 100
  102     continue
            KCOFUN(1) = min(KCOLMF,I)
            KCOFUN(2) = min(KCOLMF,I)
            goto 100
  103     continue
            JCOOVR(1) = min(JCOLMO,I)
            JCOOVR(2) = min(JCOLMO,I)
            goto 100
  104     continue
            KCOOVR(1) = min(KCOLMO,I)
            KCOOVR(2) = min(KCOLMO,I)
            goto 100
  105     continue
            JCOROT(1) = min(JCOLMR,I)
            JCOROT(2) = min(JCOLMR,I)
            goto 100
  106     continue
            KCOROT(1) = min(KCOLMR,I)
            KCOROT(2) = min(KCOLMR,I)
            goto 100
  107     continue
            JCOSEC(1) = min(JCOLMS,I)
            JCOSEC(2) = min(JCOLMS,I)
            goto 100
  108     continue
            KCOSEC(1) = min(KCOLMS,I)
            KCOSEC(2) = min(KCOLMS,I)
            goto 100
  109     continue
            ISOSLCT = I
            goto 100
  110     continue
            METHCOF = min(max(I,1),3)
            goto 100
  111     continue
            METHCOW = min(max(I,1),3)
            goto 100
        else
          call KIWI (MODE, RC1213, jummy, qummy, jummy)
          if(MODE.ne.5) go to 204
          goto 100
        end if
C     !EJECT
C---- Errors
  204 KERR = KERR+1
  203 KERR = KERR+1
  202 KERR = KERR+1
  201 KERR = KERR+1
C
      call MESHED ('CHILKAT', 1)
      write (LUEO,200) QIND
  200 format(' ','Trouble reading CO lines data control parameters. ',
     $           'List of valid control fields:'//
     $      (' ',5X,10A10))
      call CHLOE  (LUEO, QNOME, KERR)
      call ABORT
      call CARMEN
C
C---- Go home
  199 continue
C     !END
      call BYE ('CHILKAT')
C
      return
      end
