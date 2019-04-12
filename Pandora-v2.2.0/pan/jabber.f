      subroutine JABBER
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1987 Nov 17
C---- Allocates scratch storage for HOTEL.
C     !DASH
      save
C     !DASH
      integer IN, IS, MMP, MMR, MUX
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(18),MMR)
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('JABBER')
C     !BEG
      call WGET   (IS , CALLER)
C
      MMP = MMR+1
C
      IN( 1) = IS
      IN( 2) = IN( 1)+MMP
      IN( 3) = IN( 2)+MMP
      MUX    = IN( 3)+MMP
C
      call WLCK   (MUX, CALLER)
C     !END
      call BYE ('JABBER')
C
      return
      end
