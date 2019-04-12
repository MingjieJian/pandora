      subroutine RHYTHM
     $(DUMP)
C
C     Rudolf Loeser, 1983 Sep 09
C---- Determines whether a shifted-SNU calculation dump is in order.
C     !DASH
      save
C     !DASH
      integer ISNUD, KOUNT
      logical DUMP
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
      equivalence (KZQ( 74),ISNUD)
C     !DASH
      external  HI, BYE
      intrinsic mod
C
      data KOUNT /0/
C
      call HI ('RHYTHM')
C     !BEG
      DUMP = .false.
C
      if(ISNUD.gt.0) then
        KOUNT = KOUNT+1
        DUMP  = mod(KOUNT,ISNUD).eq.0
      end if
C     !END
      call BYE ('RHYTHM')
C
      return
      end
