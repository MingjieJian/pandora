      subroutine MAZDA
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1981 Nov 05
C---- Allocates scratch storage for AHURA.
C     !DASH
      save
C     !DASH
      integer IN, IS, MRR, MUX, N, NRPMX, NSHL
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(15),MRR)
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
      call HI ('MAZDA')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NRPMX*NSHL
      IN( 3) = IN( 2)+N*MRR
      IN( 4) = IN( 3)+N
      IN( 5) = IN( 4)+NRPMX
      IN( 6) = IN( 5)+N
      MUX    = IN( 6)+N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('MAZDA')
C
      return
      end
