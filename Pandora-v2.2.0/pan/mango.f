      subroutine MANGO
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1970 Feb 10
C---- Allocates scratch storage for PEEK.
C     !DASH
      save
C     !DASH
      integer I, IN, IPEX, IS, KM, LDLMX, LUEO, MK, MKL, MT, MTL, MUX,
     $        N
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
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(33),LDLMX)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external MOUSE, MESHED, MASHED, WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('MANGO')
C     !BEG
      call WGET     (IS, CALLER)
C
      call MOUSE    (N, MT)
C
      MK  = MT*KM
      MTL = MT*LDLMX
      MKL = MK*LDLMX
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+MT
      IN( 3) = IN( 2)+MT
      IN( 4) = IN( 3)+MTL
      IN( 5) = IN( 4)+MT
      IN( 6) = IN( 5)+MTL
      IN( 7) = IN( 6)+MTL
      IN( 8) = IN( 7)+MKL
      IN( 9) = IN( 8)+MK
      IN(10) = IN( 9)+MKL
      IN(11) = IN(10)+MT
C
      IN(12) = IN(11)+MT
      IN(13) = IN(12)+MK
      IN(14) = IN(13)+MKL
      MUX    = IN(14)+MK
C
      call WLCK     (MUX, CALLER)
C
      if((IPEX.lt.0).or.(IPEX.eq.11)) then
        call MESHED ('MANGO', 2)
        write (LUEO,100) CALLER
  100   format(' ',A)
        write (LUEO,101) IS,KM,LDLMX,MT,MK,MTL,MKL,MUX
  101   format(' ',10I12)
        write (LUEO,101) (IN(I),I=1,15)
        call MASHED ('MANGO')
      end if
C     !END
      call BYE ('MANGO')
C
      return
      end
