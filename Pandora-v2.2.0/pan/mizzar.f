      subroutine MIZZAR
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 2000 Jul 21
C---- Allocates scratch storage for ZIRMA.
C     !DASH
      save
C     !DASH
      integer IN, IS, KM, MUX, N, NKM
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(49),KM )
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('MIZZAR')
C     !BEG
      call WGET (IS ,CALLER)
C
      NKM = N*KM
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N
      IN( 3) = IN( 2)+N
      IN( 4) = IN( 3)+NKM
      IN( 5) = IN( 4)+NKM
      IN( 6) = IN( 5)+NKM
      MUX    = IN( 6)+KM*4
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('MIZZAR')
C
      return
      end
