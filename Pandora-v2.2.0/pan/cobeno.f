      subroutine COBENO
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1974 Dec 06
C---- Allocates scratch storage for ALTAR.
C     (This is version 2 of COBENO.)
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
      call HI ('COBENO')
C     !BEG
      call WGET (IS ,CALLER)
C
      NNL = N*NL
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N
      IN( 3) = IN( 2)+N
      IN( 4) = IN( 3)+NL
      IN( 5) = IN( 4)+NNL
      IN( 6) = IN( 5)+NNL
      IN( 7) = IN( 6)+NNL
      MUX    = IN( 7)+NNL
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('COBENO')
C
      return
      end
