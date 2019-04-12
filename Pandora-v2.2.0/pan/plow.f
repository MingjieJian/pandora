      subroutine PLOW
     $(XCBL,XLB1)
C
C     Rudolf Loeser, 1980 Aug 22
C---- Drives 'PLOUGH', to retrieve Continuum results needed for
C     Line Source Function calculations.
C     (This is version 4 of PLOW.)
C     !DASH
      save
C     !DASH
      real*8 XCBL, XLB1
      integer ITER, MMCOP, MMCSF, MMJBC, MMLAM
C     !COM
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML( 2),MMLAM)
      equivalence (MML(18),MMCSF)
      equivalence (MML(36),MMJBC)
      equivalence (MML(17),MMCOP)
C
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
C     !DASH
      external PLOUGH, HI, BYE
C
C               XLB1(Li1len), XCBL(Miklen)
      dimension XLB1(*),      XCBL(*)
C
      call HI ('PLOW')
C     !BEG
      if(ITER.eq.1) then
        call PLOUGH (XCBL, XLB1(MMLAM), XLB1(MMCSF), XLB1(MMJBC),
     $               XLB1(MMCOP))
      end if
C     !END
      call BYE ('PLOW')
C
      return
      end
