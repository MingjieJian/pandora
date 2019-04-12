      subroutine BERWYN
     $(KODE,SOURCE,NAME,I1,I2,ARRAY,LENGTH,KOUNT)
C
C     Rudolf Loeser, 1984 Jan 24
C---- Writes data for iterative summaries.
C     !DASH
      save
C     !DASH
      real*8 ARRAY
      integer I1, I2, IOVER, ITER, ITHSL, KODE, LENGTH, LITER
      logical KOUNT
      character NAME*(*), SOURCE*(*)
C     !COM
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST( 3),ITER )
      equivalence (LEST(24),LITER)
      equivalence (LEST(19),ITHSL)
      equivalence (LEST( 2),IOVER)
C
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
C     !DASH
C     !EJECT
      external CEBU, HI, BYE
C
C               ARRAY(LENGTH)
      dimension ARRAY(*)
C
      call HI ('BERWYN')
C     !BEG
      NIBIS = NIBIS+1
C
      if(NIBIS.le.MXIBIS) then
C
        if(KOUNT) then
          NITS(KODE) = NITS(KODE)+1
        end if
C
        IBNIT(NIBIS) = NITS(KODE)
        IBSRC(NIBIS) = SOURCE
        IBNAM(NIBIS) = NAME
        IBKOD(NIBIS) = KODE
        IBLEN(NIBIS) = LENGTH
        IBIN1(NIBIS) = I1
        IBIN2(NIBIS) = I2
        IBITE(NIBIS) = ITER
        IBLIT(NIBIS) = LITER
        IBITH(NIBIS) = ITHSL
        IBIOV(NIBIS) = IOVER
C
        call CEBU (ARRAY, LENGTH, IBADR(NIBIS))
      end if
C     !END
      call BYE ('BERWYN')
C
      return
      end
