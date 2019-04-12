      subroutine FRAME
     $(CHECK,N,NL,NCK,NO)
C
C     Rudolf Loeser, 1968 Jul 26
C---- Prepares plots of the CHECKs.
C     !DASH
      save
C     !DASH
      real*8 CHECK
      integer I, ITAU, IYO, J, JMAX, JMIN, JSYM, JXO, KINT, LU, MINE, N,
     $        NCK, NCKITR, NH, NL, NO, NV
      logical DOIT, PLOT
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
C
C---- IMAGE       as of 1997 Aug 21
      integer     IMALEN
      parameter   (IMALEN=65535)
      character   IMAGE*(IMALEN)
      common      /IMAGE/ IMAGE
C     Character string to hold plot images constructed by the
C     K-type line printer plotting routines;
C     but used also as a general scratch character array.
C     .
C---- IBIS        as of 2003 May 30
      integer     MXIBIS,NTMX,IBADR,IBKOD,IBNIT,IBLEN,IBIN1,IBIN2,
     $            IBITE,IBLIT,IBITH,IBIOV,NITS,NIBIS
      character   IBSRC*10, IBNAM*10
      parameter   (MXIBIS=1000)
      parameter   (NTMX=14)
C     (Remember to recompile all users when changing MXIBIS or NTMX)
      dimension   IBADR(MXIBIS), IBKOD(MXIBIS), IBNIT(MXIBIS),
     $            IBLEN(MXIBIS), IBIN1(MXIBIS), IBIN2(MXIBIS),
     $            IBITE(MXIBIS), IBLIT(MXIBIS), IBITH(MXIBIS),
     $            IBIOV(MXIBIS), IBSRC(MXIBIS), IBNAM(MXIBIS),
     $            NITS(NTMX)
      common      /IBIS1/ NIBIS, IBADR, IBKOD, IBNIT, IBLEN, IBIN1,
     $                           IBIN2, IBITE, IBLIT, IBITH, IBIOV
      common      /IBIS2/        IBSRC, IBNAM
      common      /IBIS3/ NITS
C     Control information for iterative summary data.
C
C         NITS = counts of iteration summary data records for:
C
C      1: TAU(IU,IL),    2: CHECK(L)       3: RHO(IU,IL)
C      4: RK(KOLEV)      5: ND(L)          6: RHOWT(IU,IL)
C      7: BD(KOLEV)      8: NE             9: CHI(IU,IL)
C     10: Z             11: S(IU,IL)      12: NH
C     13: TDST          14: NK
      equivalence (NITS( 2),NCKITR)
C     !DASH
C     !EJECT
      external IGEL, CANVAS, DOT, SMEAR, REAMS, MARES, HI, BYE
C
C               CHECK(N,NCK,NCKITR)
      dimension CHECK(N,NCK,*)
C
      data LU /0/
C
      call HI ('FRAME')
C     !BEG
      call REAMS          (N, NV, NH, NCK, NO, DOIT)
      if(DOIT) then
C
C----   Loop over all checks
        do 102 I = 1,NCK
          call IGEL       (CHECK, I, N, NCK, NCKITR, JMIN, JMAX, PLOT)
          if(PLOT) then
C----       Initialize plot
            call CANVAS   (IMAGE, NV, NH)
C----       Initialize interest counter
            KINT = 0
C----       Enter all points of this CHECK into the plot
C
C           Outer Loop: over all iterations
            JSYM = 0
            do 101 J = JMIN,JMAX
              JSYM = JSYM+1
C----         Initialize line (=depths) counter
              MINE = -1
C----         Initialize coordinates of endpoints of
C             current line segment
              JXO = 0
              IYO = 0
C
C----         Loop over depths
              do 100 ITAU = 1,N
                MINE = MINE+2
                call DOT  (CHECK(ITAU,I,J), IMAGE, SYMBS(JSYM), MINE,
     $                     KINT, JXO, IYO)
  100         continue
  101       continue
C
C----       If plot is interesting, print it
            if(KINT.gt.10) then
              call SMEAR  (IMAGE, NO, N, (I+2), JMIN, JMAX)
            else
              call MARES  (LU, (I+2))
            end if
          end if
  102   continue
C
      end if
C     !END
      call BYE ('FRAME')
C
      return
      end
