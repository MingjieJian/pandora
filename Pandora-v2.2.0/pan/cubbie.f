      subroutine CUBBIE
     $(I,UPUNL,KZUNL,KZAUG,KSUM,CHANGE,BANG)
C
C     Rudolf Loeser, 2004 Feb 10
C---- Increments counters, for SUMMIT.
C     !DASH
      save
C     !DASH
      integer I, KSUM, KZAUG, KZUNL, MXPPI, MXTAP
      logical BANG, CHANGE, UPUNL
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(200),MXPPI)
      equivalence (KZQ(201),MXTAP)
C     !DASH
      external HI, BYE
C
C               KZUNL(N), KZAUG(N)
      dimension KZUNL(*), KZAUG(*)
C
      call HI ('CUBBIE')
C     !BEG
      if(UPUNL) then
        KZUNL(I) = KZUNL(I)+1
      end if
C
      if((KZAUG(I).lt.MXPPI).and.(KSUM.lt.MXTAP)) then
        KZAUG(I) = KZAUG(I)+1
        KSUM     = KSUM+1
C
        CHANGE = .true.
        BANG   = .true.
      end if
C     !END
      call BYE ('CUBBIE')
C
      return
      end
