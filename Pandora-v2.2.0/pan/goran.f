      subroutine GORAN
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 2000 Nov 07
C---- Allocates scratch storage for KARDA.
C     !DASH
      save
C     !DASH
      integer IN, IS, L, LNCP, MUX, NCP
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 7),L  )
      equivalence (JZQ(44),NCP)
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('GORAN')
C     !BEG
      call WGET (IS ,CALLER)
C
      LNCP = L*NCP
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NCP
      IN( 3) = IN( 2)+LNCP
      IN( 4) = IN( 3)+LNCP
      IN( 5) = IN( 4)+LNCP
      MUX    = IN( 5)+NCP
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('GORAN')
C
      return
      end
