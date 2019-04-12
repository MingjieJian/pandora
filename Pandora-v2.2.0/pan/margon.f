      subroutine MARGON
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1983 Mar 01
C---- Allocates scratch storage for BITTERN.
C     !DASH
      save
C     !DASH
      integer IN, IS, LG, MUX, N, NNLG
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
      call HI ('MARGON')
C     !BEG
      call WGET (IS ,CALLER)
C
      NNLG = (N**2)*LG
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+NNLG
      IN( 3) = IN( 2)+NNLG
      IN( 4) = IN( 3)+LG
      MUX    = IN( 4)+NNLG
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('MARGON')
C
      return
      end
