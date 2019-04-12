      subroutine MOOD3
     $(IN,MUX)
C
C     Rudolf Loeser, 1999 Dec 03
C---- Allocates the Line Intensity data block, part 3.
C     !DASH
      save
C     !DASH
      integer IN, KM, KML, L, LF, MUX
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(49),KM )
      equivalence (JZQ( 7),L  )
      equivalence (JZQ(19),LF )
C     !DASH
      external  HI, BYE
      intrinsic max
C
      dimension IN(*)
C
      call HI ('MOOD3')
C     !BEG
      KML = KM*max(L,LF)
C
      IN( 1) = 1
      IN( 2) = IN( 1)+1
      IN( 3) = IN( 2)+KML
      IN( 4) = IN( 3)+KML
      IN( 5) = IN( 4)+KM
      IN( 6) = IN( 5)+KM
      IN( 7) = IN( 6)+KML
      IN( 8) = IN( 7)+KM
      IN( 9) = IN( 8)+KM
      IN(10) = IN( 9)+KM
C
      IN(11) = IN(10)+KM
      IN(12) = IN(11)+KML
      IN(13) = IN(12)+KML
      IN(14) = IN(13)+KM
      IN(15) = IN(14)+KM
      IN(16) = IN(15)+KML
      IN(17) = IN(16)+KM
      IN(18) = IN(17)+KM
      IN(19) = IN(18)+KM
      MUX    = IN(19)+KM
C     !END
      call BYE ('MOOD3')
C
      return
      end
