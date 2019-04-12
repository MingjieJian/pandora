      subroutine JUNTA
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1980 May 10
C---- Allocates scratch storage for MINUET.
C     !DASH
      save
C     !DASH
      integer IN, IS, ITPRD, MUX, N, NT
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 5),NT )
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
      equivalence (KZQ(214),ITPRD)
C
C---- ALVIN       as of 1995 Aug 08
      integer     LIMSCL
      parameter   (LIMSCL=11)
C     The number of TAU-scales to be printed fancily.
C     (Used in PICTURE, etc.)
C     .
C     !DASH
C     !EJECT
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('JUNTA')
C     !BEG
      call WGET (IS,  CALLER)
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+N
      IN( 3) = IN( 2)+N
      IN( 4) = IN( 3)+N*NT
      IN( 5) = IN( 4)+N
      IN( 6) = IN( 5)+N
      IN( 7) = IN( 6)+N
      IN( 8) = IN( 7)+N*(ITPRD+1)
      IN( 9) = IN( 8)+N
      MUX    = IN( 9)+N*2
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('JUNTA')
C
      return
      end
