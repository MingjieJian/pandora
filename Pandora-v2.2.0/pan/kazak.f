      subroutine KAZAK
     $(IN,IS,MUX,CALLER,NW)
C
C     Rudolf Loeser, 1981 Sep 14
C---- Allocates scratch storage for BASHKIR.
C     !DASH
      save
C     !DASH
      integer I1FX, I2FD, I3CD, I4CO, IFXDS, IN, IS, MUX, N, NNW, NW
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 78),IFXDS)
C     !DASH
C     !EJECT
      external EEL, WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('KAZAK')
C     !BEG
      call EEL  (IFXDS,I1FX,I2FD,I3CD,I4CO)
C
      call WGET (IS ,CALLER)
C
      NNW = N*NW
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NNW*I4CO
      IN( 3) = IN( 2)+N
      IN( 4) = IN( 3)+N
      IN( 5) = IN( 4)+N
      IN( 6) = IN( 5)+N
      IN( 7) = IN( 6)+N
      IN( 8) = IN( 7)+N
      IN( 9) = IN( 8)+N*N
      IN(10) = IN( 9)+N
C
      IN(11) = IN(10)+N
      IN(12) = IN(11)+N
      IN(13) = IN(12)+NNW*I1FX
      IN(14) = IN(13)+NNW*I2FD
      MUX    = IN(14)+NNW*I3CD
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('KAZAK')
C
      return
      end
