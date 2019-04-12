      subroutine IRKUTSK
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1981 Sep 15
C---- Allocates scratch storage for YENISEI.
C     !DASH
      save
C     !DASH
      integer IN, IS, MRR, MUX, N, NRPMX
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
      equivalence (LEST( 8),NRPMX)
C     !DASH
      external  WGET, WLCK, HI, BYE
      intrinsic max
C
      dimension IN(*)
C
      call HI ('IRKUTSK')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+(N+1)
      IN( 3) = IN( 2)+N
      IN( 4) = IN( 3)+MRR
      IN( 5) = IN( 4)+MRR
      IN( 6) = IN( 5)+MRR
      IN( 7) = IN( 6)+NRPMX
      IN( 8) = IN( 7)+NRPMX
      IN( 9) = IN( 8)+MRR
      IN(10) = IN( 9)+max(N,MRR)
C
      IN(11) = IN(10)+N
      IN(12) = IN(11)+NRPMX
      MUX    = IN(12)+NRPMX
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('IRKUTSK')
C
      return
      end
