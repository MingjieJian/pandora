      subroutine JUNCO
     $(N,RK,BD,Z,TDST,KIBI,NO,PRAT)
C
C     Rudolf Loeser, 1984 Jan 25
C---- Supervises some iterative summaries.
C     !DASH
      save
C     !DASH
      real*8 BD, PRAT, RK, TDST, Z
      integer KIBI, KOLEV, N, NBKITR, NO, NRKITR, NTDITR, NZZITR
      logical TR
      character TITLE*15
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 33),KOLEV)
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
      equivalence (NITS( 4),NRKITR)
      equivalence (NITS( 7),NBKITR)
      equivalence (NITS(10),NZZITR)
      equivalence (NITS(13),NTDITR)
C     !DASH
C     !EJECT
      external CROW, HI, BYE
C
C               Z(N,NZZITR), BD(N,NBKITR), TDST(N,NTDITR), PRAT(N),
      dimension Z(*),        BD(*),        TDST(*),        PRAT(*),
C
C               RK(N,NRKITR)
     $          RK(*)
C
      data TR /.true./
C
      call HI ('JUNCO')
C     !BEG
      if(NRKITR.gt.1) then
        write (TITLE,100) KOLEV
  100   format('RK',I2,11X)
        call CROW (N,NRKITR,0,RK  ,TITLE,NO,KIBI,1,TR,PRAT)
      end if
C
      if(NBKITR.gt.1) then
        write (TITLE,101) KOLEV
  101   format('BD',I2,11X)
        call CROW (N,NBKITR,0,BD  ,TITLE,NO,KIBI,1,TR,PRAT)
      end if
C
      if(NZZITR.gt.1) then
        TITLE = 'Z              '
        call CROW (N,NZZITR,1,Z   ,TITLE,NO,KIBI,1,TR,PRAT)
      end if
C
      if(NTDITR.gt.1) then
        TITLE = 'TDST           '
        call CROW (N,NTDITR,1,TDST,TITLE,NO,KIBI,1,TR,PRAT)
      end if
C     !END
      call BYE ('JUNCO')
C
      return
      end
