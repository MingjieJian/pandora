      subroutine CYNX
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 2003 Jun 24
C---- Allocates scratch storage, for ARETE.
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
      call HI ('CYNX')
C     !BEG
      call WGET (IS,  CALLER)
C
      NNSL = N*NSL
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+NNSL
      IN( 3) = IN( 2)+NNSL
      IN( 4) = IN( 3)+NNSL
      IN( 5) = IN( 4)+NNSL
      IN( 6) = IN( 5)+NNSL
      IN( 7) = IN( 6)+NNSL
      MUX    = IN( 7)+NNSL
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('CYNX')
C
      return
      end
