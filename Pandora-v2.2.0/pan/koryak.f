      subroutine KORYAK
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1985 Jan 22
C---- Allocates scratch storage for TOBOL.
C     (This is version 2 of KORYAK.)
C     !DASH
      save
C     !DASH
      integer IBL, IN, IS, KM, MUX, N, NKM
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(49),KM )
C
C---- PERBLOC     as of 2005 Jan 21
      integer     LPDLEN,LPD
      dimension   LPD(15)
      common      /PERBLOC/ LPDLEN,LPD
C     This is the "DIANA" Data Block index, for the calculation of
C     line source functions in a static atmosphere.
C
C---- ORIBLOC     as of 2005 Jan 21
      integer     LODLEN,LOD
      dimension   LOD(17)
      common      /ORIBLOC/ LODLEN,LOD
C     This is the "ORION" Data Block index, for the calculation
C     of line source functions in an expanding atmosphere.
C     !DASH
C     !EJECT
      external  WGET, WLCK, HI, BYE
      intrinsic max
C
      dimension IN(*)
C
      call HI ('KORYAK')
C     !BEG
      call WGET (IS ,CALLER)
C
      IBL = max(LPDLEN,LODLEN)
      NKM = N*KM
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NKM
      IN( 3) = IN( 2)+NKM
      IN( 4) = IN( 3)+IBL
      IN( 5) = IN( 4)+NKM
      IN( 6) = IN( 5)+N
      IN( 7) = IN( 6)+KM
      IN( 8) = IN( 7)+KM
      IN( 9) = IN( 8)+N
      IN(10) = IN( 9)+NKM
      MUX    = IN(10)+N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('KORYAK')
C
      return
      end
