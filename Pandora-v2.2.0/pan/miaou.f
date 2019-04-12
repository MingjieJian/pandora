      subroutine MIAOU
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1987 Mar 24
C---- Allocates scratch storage for RAGNAR.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N, NDT, NN
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(21),NDT)
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('MIAOU')
C     !BEG
      call WGET (IS ,CALLER)
C
      NN = N*NDT
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N
      IN( 3) = IN( 2)+N
      IN( 4) = IN( 3)+N
      IN( 5) = IN( 4)+NN
      MUX    = IN( 5)+NN
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('MIAOU')
C
      return
      end
