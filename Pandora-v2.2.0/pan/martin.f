      subroutine MARTIN
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1982 Apr 08
C---- Allocates scratch storage for TOTILA.
C     !DASH
      save
C     !DASH
      integer IN, IS, KM, MRR, MUX, N, NR, NS
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
      intrinsic max
C
      dimension IN(*)
C
      call HI ('MARTIN')
C     !BEG
      call WGET (IS ,CALLER)
C
      NR = N+max((MRR-1),0)
      NS = max(NR,9)*max(KM,13)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NS
      IN( 3) = IN( 2)+NR*KM
      IN( 4) = IN( 3)+NR
      MUX    = IN( 4)+NR
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('MARTIN')
C
      return
      end
