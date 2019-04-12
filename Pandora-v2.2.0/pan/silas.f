      subroutine SILAS
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1993 Sep 10
C---- Allocates scratch storage for HIRAM.
C     !DASH
      save
C     !DASH
      integer IN, IS, KAVNP, KAVNT, KAVNZ, KWA, MUX, N, NTP, NTQ
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(57),KWA)
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
      equivalence (KZQ(142),KAVNP)
      equivalence (KZQ(141),KAVNT)
      equivalence (KZQ(143),KAVNZ)
C
C---- KNTKUPL     as of 1993 Sep 15
      integer     KNTKU
      parameter   (KNTKU=5)
C     Control parameter for "Line" opacity plots.
C     (Used in CADMOS, MAYA, SILAS.)
C     .
C     !DASH
      external  WGET, WLCK, HI, BYE
      intrinsic max
C
      dimension IN(*)
C     !EJECT
C
      call HI ('SILAS')
C     !BEG
      call WGET (IS ,CALLER)
C
      NTP = KAVNT*KAVNP
      NTQ = max(NTP,KAVNZ)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+KAVNZ
      IN( 3) = IN( 2)+KAVNZ
      IN( 4) = IN( 3)+KAVNT
      IN( 5) = IN( 4)+KAVNP
      IN( 6) = IN( 5)+NTP
      IN( 7) = IN( 6)+N
      IN( 8) = IN( 7)+KWA*KNTKU
      IN( 9) = IN( 8)+NTQ
      MUX    = IN( 9)+N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('SILAS')
C
      return
      end
