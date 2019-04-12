      subroutine LYNX
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1978 May 04
C---- Allocates scratch storage, for WILY.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N, NNSL, NSL
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(40),NSL)
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('LYNX')
C     !BEG
      call WGET (IS ,CALLER)
C
      NNSL = N*NSL
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NNSL
      IN( 3) = IN( 2)+NNSL
      IN( 4) = IN( 3)+N
      MUX    = IN( 4)+N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('LYNX')
C
      return
      end
