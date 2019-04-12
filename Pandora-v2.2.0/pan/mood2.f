      subroutine MOOD2
     $(IN,MUX)
C
C     Rudolf Loeser, 1999 Dec 03
C---- Allocates the Line Intensity data block, part 2.
C     !DASH
      save
C     !DASH
      integer IN, KM, MUX, N, NKM
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(49),KM )
C     !DASH
      external HI, BYE
C
      dimension IN(*)
C
      call HI ('MOOD2')
C     !BEG
      NKM  = N*KM
C
      IN( 1) = 1
      IN( 2) = IN( 1)+1
      IN( 3) = IN( 2)+NKM
      IN( 4) = IN( 3)+NKM
      IN( 5) = IN( 4)+NKM
      IN( 6) = IN( 5)+NKM
      IN( 7) = IN( 6)+NKM
      MUX    = IN( 7)+NKM
C     !END
      call BYE ('MOOD2')
C
      return
      end
