      subroutine URE
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1983 Jan 27
C---- Drives Passive Jnu shuffling.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IW, IX, NPROG, NPT
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
      equivalence (LEST( 7),NPT  )
C     !DASH
      external LOGIN, BIBI, LOGOUT, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      data NPROG /29/
C
      call HI ('URE')
C     !BEG
      if(NPT.gt.0) then
        call LOGIN  (NPROG)
        call BIBI   (X,W)
        call LOGOUT (NPROG)
      end if
C     !END
      call BYE ('URE')
C
      return
      end
