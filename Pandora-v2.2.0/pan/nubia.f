      subroutine NUBIA
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1970 Feb 16
C---- Allocates scratch storage for NOVA.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, NL, NL2, NLM, NLM2
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 2),NL )
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('NUBIA')
C     !BEG
      call WGET (IS,  CALLER)
C
      NL2  = NL**2
      NLM  = NL-1
      NLM2 = NLM**2
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+NLM2
      IN( 3) = IN( 2)+NLM
      IN( 4) = IN( 3)+NL2
      IN( 5) = IN( 4)+NLM
      IN( 6) = IN( 5)+NLM2
      MUX    = IN( 6)+NLM
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('NUBIA')
C
      return
      end
