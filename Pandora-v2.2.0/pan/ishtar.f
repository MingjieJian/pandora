      subroutine ISHTAR
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1970 Jan 29
C---- Allocates scratch storage for GRIS.
C     !DASH
      save
C     !DASH
      integer IN, IS, KM, MKM, MRR, MUX, N, NKM
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(49),KM )
      equivalence (JZQ(15),MRR)
C     !DASH
      external  WGET, WLCK, HI, BYE
C
      dimension IN(*)
C     !EJECT
C
      call HI ('ISHTAR')
C     !BEG
      call WGET (IS ,CALLER)
C
      NKM = N*KM
      MKM = MRR*KM
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N
      IN( 3) = IN( 2)+NKM
      IN( 4) = IN( 3)+N
      IN( 5) = IN( 4)+MKM
      IN( 6) = IN( 5)+KM
      IN( 7) = IN( 6)+KM
      IN( 8) = IN( 7)+KM
      IN( 9) = IN( 8)+N
      IN(10) = IN( 9)+MRR
C
      IN(11) = IN(10)+N
      IN(12) = IN(11)+KM
      IN(13) = IN(12)+KM
      IN(14) = IN(13)+MRR
      IN(15) = IN(14)+KM
      IN(16) = IN(15)+NKM
      IN(17) = IN(16)+NKM
      IN(18) = IN(17)+KM
      IN(19) = IN(18)+MKM
      IN(20) = IN(19)+MKM
C
      IN(21) = IN(20)+NKM
      IN(22) = IN(21)+MKM
      IN(23) = IN(22)+KM
      MUX    = IN(23)+KM
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('ISHTAR')
C
      return
      end
