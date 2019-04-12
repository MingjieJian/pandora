      subroutine IVOR
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1983 Feb 18
C---- Allocates scratch storage for ERITH.
C     !DASH
      save
C     !DASH
      integer IN, IS, LG, MUX, N, NN, NNLG
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(34),LG )
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('IVOR')
C     !BEG
      call WGET (IS ,CALLER)
C
      NN   = N**2
      NNLG = NN*LG
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NNLG
      IN( 3) = IN( 2)+NNLG
      IN( 4) = IN( 3)+N*LG
      IN( 5) = IN( 4)+NN
      IN( 6) = IN( 5)+N
      IN( 7) = IN( 6)+N
      IN( 8) = IN( 7)+N
      MUX    = IN( 8)+N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('IVOR')
C
      return
      end
