      subroutine ANUBIS
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 2003 Nov 19
C---- Allocates scratch storage for VAMOS.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N, NL, NL2, NLM, NLM2
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
      call HI ('ANUBIS')
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
      call BYE ('ANUBIS')
C
      return
      end
