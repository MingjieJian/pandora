      subroutine LAPIS
     $(IN,MUX)
C
C     Rudolf Loeser, 1981 Nov 13
C---- Allocates the Orion Data Block.
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
      call HI ('LAPIS')
C     !BEG
      NN = N**2
C
      IN( 1) = 1
      IN( 2) = IN( 1)+1
      IN( 3) = IN( 2)+NN
      IN( 4) = IN( 3)+N
      IN( 5) = IN( 4)+N
      IN( 6) = IN( 5)+N
      IN( 7) = IN( 6)+NN
      IN( 8) = IN( 7)+N
      IN( 9) = IN( 8)+N
      IN(10) = IN( 9)+N
      IN(11) = IN(10)+N
C
      IN(12) = IN(11)+N
      IN(13) = IN(12)+1
      IN(14) = IN(13)+1
      IN(15) = IN(14)+NN
      IN(16) = IN(15)+N
      IN(17) = IN(16)+N
      MUX    = IN(17)+N
C     !END
      call BYE ('LAPIS')
C
      return
      end
