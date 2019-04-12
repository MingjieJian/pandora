      subroutine TOLUGO
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1997 Aug 08
C---- Allocates scratch storage for COLUGO.
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
      call HI ('TOLUGO')
C     !BEG
      call WGET (IS ,CALLER)
C
      NNL = N*NL
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N
      IN( 3) = IN( 2)+NNL
      IN( 4) = IN( 3)+NNL
      IN( 5) = IN( 4)+NNL
      MUX    = IN( 5)+N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('TOLUGO')
C
      return
      end
