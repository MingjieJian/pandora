      subroutine SMELT
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1987 Oct 19
C---- Allocates scratch storage for GRUNCH.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N, NL, NZ
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
      call HI ('SMELT')
C     !BEG
      call WGET (IS ,CALLER)
C
      NZ   =N*(NL**2)
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+NZ
      IN( 3) = IN( 2)+N
      IN( 4) = IN( 3)+N
      IN( 5) = IN( 4)+N
      MUX    = IN( 5)+N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('SMELT')
C
      return
      end
