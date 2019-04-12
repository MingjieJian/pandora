      subroutine RUNINA
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1997 Aug 08
C---- Allocates scratch storage for CUNINA.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N, NL, NNL
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('RUNINA')
C     !BEG
      call WGET (IS , CALLER)
C
      NNL = N*NL
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N
      IN( 3) = IN( 2)+NNL
      IN( 4) = IN( 3)+N
      MUX    = IN( 4)+NNL
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('RUNINA')
C
      return
      end
