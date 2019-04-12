      subroutine GANDER
     $(X)
C
C     Rudolf Loeser, 1976 Apr 27
C---- Computes and exhibits geometrical correction terms matrix.
C     !DASH
      save
C     !DASH
      real*8 DELTA, X
      integer JJGDT, JJGDZ, JJZ, KGDT, KODE, N, NGDZ
      character TITLE*100
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(191),JJGDT)
      equivalence (IZOQ(190),JJGDZ)
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
      equivalence (LEST(45),NGDZ )
      equivalence (LEST(46),KGDT )
C     !DASH
      external GOOSE, MOVE1, ARISOD, HI, BYE
C
      dimension X(*)
C
      data TITLE /'Complete Geometrical Corrections terms matrix'/
      data DELTA /1.D-6/
C
      call HI ('GANDER')
C     !BEG
      call ARISOD  (X(JJGDZ), NGDZ, X(JJZ), N, DELTA, KODE)
      if(KODE.ne.1) then
        NGDZ = N
        call MOVE1 (X(JJZ), N, X(JJGDZ))
        call GOOSE (X(JJGDZ), NGDZ, X(JJGDT), KGDT, TITLE)
      end if
C     !END
      call BYE ('GANDER')
C
      return
      end
