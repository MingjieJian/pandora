      subroutine NOOTKA
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1983 Mar 15
C---- Allocates scratch storage for FERREX.
C     (This is version 2 of NOOTKA.)
C     !DASH
      save
C     !DASH
      integer IN, IS, JR, JZ, MUX, N, NRPMX, NSHL
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
      equivalence (LEST( 4),NSHL )
      equivalence (LEST( 8),NRPMX)
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('NOOTKA')
C     !BEG
      call WGET (IS ,CALLER)
C
      JZ = NSHL*(N**2)
      JR = NRPMX**2
C
      IN( 1) = IS
      IN( 2) = IN( 1)+JZ
      IN( 3) = IN( 2)+JR
      IN( 4) = IN( 3)+JZ
      IN( 5) = IN( 4)+JR
      IN( 6) = IN( 5)+JR
      MUX    = IN( 6)+JR
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('NOOTKA')
C
      return
      end
