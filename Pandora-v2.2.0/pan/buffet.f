      subroutine BUFFET
     $(RHOWT,N)
C
C     Rudolf Loeser, 1978 Nov 08
C---- Fiddles with interpolated RHWT, for BULL.
C     !DASH
      save
C     !DASH
      real*8 RHOWT, WMN, WMX
      integer N
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
      equivalence (RZQ( 43),WMN  )
      equivalence (RZQ( 55),WMX  )
C     !DASH
      external BOUNDS, HI, BYE
C
C               RHOWT(N)
      dimension RHOWT(*)
C
      call HI ('BUFFET')
C     !BEG
      call BOUNDS (N,WMN,RHOWT,WMX)
C     !END
      call BYE ('BUFFET')
C
      return
      end
