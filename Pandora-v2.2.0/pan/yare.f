      subroutine YARE
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1975 May 21
C---- Drives the writing of restart data files.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IW, IX, NONC, NPROG
      logical GO, UPJNU
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
      equivalence (LEST(29),NONC )
C     !DASH
      external DONKEY, LOGIN, PAULUS, LOGOUT, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      data NPROG /18/
C
      call HI ('YARE')
C     !BEG
      call DONKEY   (GO,UPJNU)
C
      if(GO) then
        call LOGIN  (NPROG)
        call PAULUS (X,IX,W,IW,UPJNU,NONC)
        call LOGOUT (NPROG)
      end if
C     !END
      call BYE ('YARE')
C
      return
      end
