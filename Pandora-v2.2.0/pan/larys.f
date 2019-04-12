      subroutine LARYS
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 2003 Mar 14
C---- Allocates scratch storage for CRYSTAL.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N, NL, NL2
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 2),NL )
      equivalence (JZQ( 1),N  )
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('LARYS')
C     !BEG
      call WGET (IS , CALLER)
C
      NL2 = NL*NL
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NL
      IN( 3) = IN( 2)+NL2
      IN( 4) = IN( 3)+NL2
      MUX    = IN( 4)+N
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('LARYS')
C
      return
      end
