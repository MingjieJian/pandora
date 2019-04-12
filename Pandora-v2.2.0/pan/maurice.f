      subroutine MAURICE
     $(IN,IS,MUX,CALLER,NW)
C
C     Rudolf Loeser, 1993 Jun 15
C---- Alllocates scratch storage for GRIFFIN.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N, NRPMX, NW, NWNZ, NZE
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(56),NZE)
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
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C     !EJECT
C
      call HI ('MAURICE')
C     !BEG
      call WGET (IS ,CALLER)
C
      NWNZ = NW*NZE
C
      IN( 1) = IS
      IN( 2) = IN( 1)+(N+1)
      IN( 3) = IN( 2)+N
      IN( 4) = IN( 3)+N
      IN( 5) = IN( 4)+NRPMX
      IN( 6) = IN( 5)+NRPMX
      IN( 7) = IN( 6)+NRPMX
      IN( 8) = IN( 7)+NRPMX
      IN( 9) = IN( 8)+NWNZ
      IN(10) = IN( 9)+NWNZ
C
      IN(11) = IN(10)+N
      IN(12) = IN(11)+N
      IN(13) = IN(12)+NW
      IN(14) = IN(13)+NWNZ
      IN(15) = IN(14)+NZE
      MUX    = IN(15)+N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('MAURICE')
C
      return
      end
