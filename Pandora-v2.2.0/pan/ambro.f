      subroutine AMBRO
     $(X,W,N,NAB,NCP,LWNT,KWC,NP,NT,NV,LU,KOMPO,KODNT,INDEX,FABD,KNT,
     $ NKA,HND,H2N,TE,XNE,V,WAVCO,INWVC,ARRCO,BANDL,BANDU,ALB,DEN,
     $ PGS,C01,C02,C03,C04,C05,C06,ITJ,IPJ,IVJ,KAPSMP,TABP,TABT,TABV,
     $ CLOP,COMP,MULT,WAVE)
C
C     Rudolf Loeser, 1983 Jul 05
C---- Sets up Composite Line Opacity Data Array.
C     !DASH
      save
C     !DASH
      real*8 ALB, ARRCO, BANDL, BANDU, C01, C02, C03, C04, C05, C06,
     $       CLOP, COMP, DEN, FABD, H2N, HND, PGS, TABP, TABT, TABV, TE,
     $       V, W, WAVCO, WAVE, X, XNE
      integer I, INDEX, INWVC, IPJ, ITJ, IVJ, KAPSMP, KMULT, KNT, KODNT,
     $        KOMPO, KWC, LU, LWNT, MULT, N, NAB, NCP, NKA, NP, NR, NT,
     $        NV
      logical DUMP
C     !DASH
C     !EJECT
      external GASSER, NINEVEH, LYNECO, ELYMAIS, ZIKIRTU, ANSHAN, LACK,
     $         GIYAN, RUSAS, IZZARD, CHECKER, ZAB, MASHED, HI, BYE
C
      dimension X(*), W(*)
C
C               WAVCO(NCP), INWVC(NCP), HND(N), H2N(N), TE(N), CLOP(N),
      dimension WAVCO(*),   INWVC(*),   HND(*), H2N(*), TE(*), CLOP(*),
C
C               XNE(N), TABV(NV), ARRCO(NCP,N), BANDL(NAB), BANDU(NAB),
     $          XNE(*), TABV(*),  ARRCO(*),     BANDL(*),   BANDU(*),
C
C               ALB(N), DEN(N), PGS(N), C01(N), C02(N), C03(N), IVJ(N),
     $          ALB(*), DEN(*), PGS(*), C01(*), C02(*), C03(*), IVJ(*),
C
C               C04(N), C05(N), C06(N), ITJ(N), IPJ(N), TABT(NT), V(N),
     $          C04(*), C05(*), C06(*), ITJ(*), IPJ(*), TABT(*),  V(*),
C
C               KAPSMP(NP,NT,NV), TABP(NP), INDEX(KNT), MULT(NVP),
     $          KAPSMP(*),        TABP(*),  INDEX(*),   MULT(*),
C
C               COMP(NCP)
     $          COMP(*)
C
      call HI ('AMBRO')
C     !BEG
C---- Get gas parameters
      call GASSER  (X, N, DEN, PGS, W)
C---- Get pressure, temperature and velocity grids
C     (from data input file)
      call NINEVEH (KOMPO, NP, TABP, NT, TABT, NV, TABV)
C---- Compute interpolation coefficients
      call LYNECO  (NP, NT, NV, TABP, TABT, TABV, N, PGS, TE, V,
     $              C01, C02, C03, C04, C05, C06, IPJ, ITJ, IVJ)
C---- Dump (if needed)
      call ELYMAIS (KODNT, NP, TABP, NT, TABT, NV, TABV, N, C01, C02,
     $              C03, C04, C05, C06, IPJ, ITJ, IVJ, NCP, INWVC,
     $              WAVCO, DUMP, 'AMBRO')
C     !EJECT
C---- Loop over all relevant wavelengths, (selecting appropriate
C     records from data input file, as indicated by INWVC)
      NR = 0
      do 100 I = 1,NCP
C----   Get KAPSMP array from appropriate data record
        call ZIKIRTU (KOMPO, NR, KWC, INWVC(I), KAPSMP, NP, NT, NV)
C----   Dump (if needed)
        call ANSHAN  (DUMP, KODNT, I, WAVCO(I), INWVC(I), KAPSMP,
     $                NP, NT, NV)
C----   Get Composite Line Opacity
        call GIYAN   (I, N, NP, NT, NV, NCP, IPJ, ITJ, IVJ, KAPSMP,
     $                C01, C02, C03, C04, C05, C06, ARRCO, DEN,
     $                CLOP, FABD)
  100 continue
      if(DUMP) then
        call MASHED  ('AMBRO')
      end if
C---- Return data file
      call LACK      (KOMPO, 0)
C
C---- Determine whether opacity multipliers have any effect
      call IZZARD    (WAVCO, NCP, MULT, BANDL, BANDU, NAB, X, KMULT)
C---- Print
      call RUSAS     (LU, TE, XNE, HND, H2N, DEN, PGS, ALB, V, WAVCO,
     $                ARRCO, IPJ, ITJ, IVJ, N, NCP, MULT, NAB, KMULT,
     $                FABD, NKA, WAVE, LWNT)
C---- Plot
      call ZAB       (LU, INDEX, KNT, NAB, BANDL, BANDU, N, WAVCO,
     $                ARRCO, COMP, NCP)
C---- Checksums
      call CHECKER   (ARRCO, 1, (NCP*N), 'Composite Line Opacity')
C     !END
      call BYE ('AMBRO')
C
      return
      end
