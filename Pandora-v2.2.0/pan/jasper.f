      subroutine JASPER
     $(IN,MUX)
C
C     Rudolf Loeser, 1977 Aug 19
C---- Allocates the Diana Data Block.
C     !DASH
      save
C     !DASH
      integer IN, MUX, N, NN
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C     !DASH
      external HI, BYE
C
      dimension IN(*)
C
      call HI ('JASPER')
C     !BEG
      NN = N**2
C
      IN( 1) = 1
C
      IN( 2) = IN( 1)+1
      IN( 3) = IN( 2)+N
      IN( 4) = IN( 3)+N
      IN( 5) = IN( 4)+N
      IN( 6) = IN( 5)+N
      IN( 7) = IN( 6)+N
      IN( 8) = IN( 7)+NN
      IN( 9) = IN( 8)+N
      IN(10) = IN( 9)+NN
      IN(11) = IN(10)+1
C
      IN(12) = IN(11)+1
      IN(13) = IN(12)+NN
      IN(14) = IN(13)+N
      IN(15) = IN(14)+N
      MUX    = IN(15)+N
C     !END
      call BYE ('JASPER')
C
      return
      end
