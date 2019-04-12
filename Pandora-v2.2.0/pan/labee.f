      subroutine LABEE
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 2000 Jun 29
C---- Allocates scratch storage for BEVEL.
C     !DASH
      save
C     !DASH
      integer I, IN, IPEX, IS, KM, LKM, LUEO, MKM, MRR, MUX, N, NKM
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
      external  WGET, WLCK, MESHED, MASHED, HI, BYE
      intrinsic max
C
      dimension IN(*)
C
      call HI ('LABEE')
C     !BEG
      call WGET (IS,  CALLER)
C
      NKM = N*KM
      MKM = MRR*KM
      LKM = max(N,MRR)*KM
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+N
      IN( 3) = IN( 2)+MRR
      IN( 4) = IN( 3)+LKM
      IN( 5) = IN( 4)+NKM
      IN( 6) = IN( 5)+NKM
      IN( 7) = IN( 6)+MKM
      IN( 8) = IN( 7)+MKM
      IN( 9) = IN( 8)+KM
      MUX    = IN( 9)+KM
C
      call WLCK (MUX, CALLER)
C
      if((IPEX.lt.0).or.(IPEX.eq.4)) then
        call MESHED ('LABEE', 3)
        write (LUEO,100) IS,N,MRR,KM,NKM,MKM,LKM,MUX
  100   format(' ','IS =',I10,', N =',I6,', MRR =',I6,', KM =',I6,
     $             ', NKM =',I10,', MKM =',I10,', LKM =',I10,
     $             ', MUX =',I10)
        write (LUEO,101) (IN(I),I=1,9)
  101   format(' ','IN =',10I12)
        call MASHED ('LABEE')
      end if
C     !END
      call BYE ('LABEE')
C
      return
      end
