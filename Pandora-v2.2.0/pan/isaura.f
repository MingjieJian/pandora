      subroutine ISAURA
     $(IN,IS,MUX,CALLER,N)
C
C     Rudolf Loeser, 1981 Sep 02
C---- Allocates scratch storage for YMUIR/YMIR.
C     (This is version 2 of ISAURA.)
C     !DASH
      save
C     !DASH
      integer IN, IPEX, IS, J, KM, LDLMX, LUEO, MUX, N, NKLD, NLD
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
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
      external WGET, WLCK, MESHED, MASHED, HI, BYE
C
      dimension IN(*)
C
      call HI ('ISAURA')
C     !BEG
      call WGET     (IS, CALLER)
C
      NLD  = N*LDLMX
      NKLD = NLD*KM
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+NLD
      IN( 3) = IN( 2)+NLD
      IN( 4) = IN( 3)+NKLD
      MUX    = IN( 4)+NKLD
C
      call WLCK     (MUX, CALLER)
C
      if((IPEX.lt.0).or.(IPEX.eq.4)) then
        call MESHED ('ISAURA', 2)
        write (LUEO,100) IS,N,KM,LDLMX,NLD,NKLD,MUX
  100   format(' ',10I12)
        write (LUEO,100) (IN(J),J=1,4)
        call MASHED ('ISAURA')
      end if
C     !END
      call BYE ('ISAURA')
C
      return
      end
