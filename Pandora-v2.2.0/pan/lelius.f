      subroutine LELIUS
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1983 Feb 24
C---- Allocates scratch storage for GUNBERT.
C     !DASH
      save
C     !DASH
      integer IN, IRA, IS, LDLMX, MUX, N, NLD, NRPMX, NX2
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
      equivalence (LEST(33),LDLMX)
      equivalence (LEST( 8),NRPMX)
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C     !EJECT
C
      call HI ('LELIUS')
C     !BEG
      call WGET (IS,  CALLER)
C
      NX2 = NRPMX**2
      IRA = (N+3)*NRPMX
      NLD = NRPMX*LDLMX
C
      IN( 1) = IS
      IN( 2) = IN( 1)+IRA
      IN( 3) = IN( 2)+IRA
      IN( 4) = IN( 3)+IRA
      IN( 5) = IN( 4)+NLD
      IN( 6) = IN( 5)+NRPMX
      IN( 7) = IN( 6)+NRPMX
      IN( 8) = IN( 7)+NRPMX
      IN( 9) = IN( 8)+NRPMX
      IN(10) = IN( 9)+NRPMX
      IN(11) = IN(10)+NRPMX
C
      IN(12) = IN(11)+NRPMX
      IN(13) = IN(12)+NLD
      IN(14) = IN(13)+NLD
      IN(15) = IN(14)+NLD
      IN(16) = IN(15)+NX2
      IN(17) = IN(16)+NRPMX
      IN(18) = IN(17)+NRPMX
      IN(19) = IN(18)+NX2
      IN(20) = IN(19)+NX2
      IN(21) = IN(20)+NRPMX
C
      IN(22) = IN(21)+NLD
      IN(23) = IN(22)+NRPMX
      IN(24) = IN(23)+NRPMX
      IN(25) = IN(24)+NX2
      MUX    = IN(25)+N
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('LELIUS')
C
      return
      end
