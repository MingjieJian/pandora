      subroutine IDAS
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1991 Dec 17
C---- Allocates scratch storage for VELVET.
C     !DASH
      save
C     !DASH
      integer IN, IS, LDLMX, MUX
      character CALLER*(*)
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
      equivalence (LEST(33),LDLMX)
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('IDAS')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+1
      IN( 3) = IN( 2)+LDLMX
      IN( 4) = IN( 3)+LDLMX
      IN( 5) = IN( 4)+LDLMX
      IN( 6) = IN( 5)+LDLMX
      MUX    = IN( 6)+LDLMX
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('IDAS')
C
      return
      end
