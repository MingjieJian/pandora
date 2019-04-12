      subroutine KAREN
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1981 Sep 16
C---- Allocates scratch storage for GRAU.
C     !DASH
      save
C     !DASH
      integer I, IN, IPEX, IS, KM, LUEO, MUX, N
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
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 18),IPEX )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external WGET, WLCK, MESHED, MASHED, HI, BYE
C
      dimension IN(*)
C
      call HI ('KAREN')
C     !BEG
      call WGET     (IS, CALLER)
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+KM
      IN( 3) = IN( 2)+KM
      IN( 4) = IN( 3)+KM
      IN( 5) = IN( 4)+KM
      IN( 6) = IN( 5)+KM
      IN( 7) = IN( 6)+N
      IN( 8) = IN( 7)+KM
      IN( 9) = IN( 8)+KM
      MUX    = IN( 9)+KM
C
      call WLCK     (MUX, CALLER)
C
      if((IPEX.lt.0).or.(IPEX.eq.4)) then
        call MESHED ('KAREN', 2)
        write (LUEO,100) IS,N,KM,MUX
  100   format(' ',10I12)
        write (LUEO,100) (IN(I),I=1,9)
        call MASHED ('KAREN')
      end if
C     !END
      call BYE ('KAREN')
C
      return
      end
