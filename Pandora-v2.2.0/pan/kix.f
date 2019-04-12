      subroutine KIX
     $(X,RKI,IQRK,NO)
C
C     Rudolf Loeser, 1980 Mar 12
C---- Drives K-Shell Ionization calculation.
C     !DASH
      save
C     !DASH
      real*8 RKI, X
      integer IQRK, JJPKS, JJQIN, KSHEL, N, NO, NSL
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(40),NSL)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 82),JJQIN)
      equivalence (IZOQ( 88),JJPKS)
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
      equivalence (LEST( 1),KSHEL)
C     !DASH
      external POOF, HI, BYE
C
      dimension X(*)
C
C               RKI(N,NSL), IQRK(NSL)
      dimension RKI(*),     IQRK(*)
C
      call HI ('KIX')
C     !BEG
      if(KSHEL.gt.0) then
        call POOF (N,NSL,RKI,IQRK,X(JJQIN),X(JJPKS),NO)
      end if
C     !END
      call BYE ('KIX')
C
      return
      end
