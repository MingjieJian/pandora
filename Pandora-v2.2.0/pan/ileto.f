      subroutine ILETO
     $(X,GDIL,WN,N,TITLE,DUMP)
C
C     Rudolf Loeser, 1989 Dec 06
C---- Applies geometrical corrections to the WN matrix, (if needed).
C     !DASH
      save
C     !DASH
      real*8 WN, X
      integer JJGDT, JJGDZ, JJZ, KGDT, N
      logical DUMP, GDIL
      character TITLE*(*)
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(190),JJGDZ)
      equivalence (IZOQ(191),JJGDT)
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
      equivalence (LEST(46),KGDT )
C     !DASH
      external UPSILON, IOTA, HI, BYE
C
      dimension X(*)
C
C               WN(N,N)
      dimension WN(*)
C
      call HI ('ILETO')
C     !BEG
      if(GDIL) then
        call UPSILON (X(JJZ), N, X(JJGDZ), X(JJGDT), WN, TITLE)
        if((KGDT.eq.1).and.DUMP) then
          call IOTA  (WN, N, TITLE, 'WN after Geometrical Dilution')
        end if
      end if
C     !END
      call BYE ('ILETO')
C
      return
      end
