      subroutine MERGINI
     $(HYDR,KS)
C
C     Rudolf Loeser, 1986 Jul 15
C---- Sets up the cooling rates component switches for Hydrogen runs.
C     !DASH
      save
C     !DASH
      integer I, KHFFS, KOOLS, KS
      logical HYDR
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
      equivalence (KZQ( 86),KOOLS)
      equivalence (KZQ(175),KHFFS)
C     !DASH
      external  ZEROI, HI, BYE
      intrinsic mod, min, max
C
      dimension KS(6)
C
      call HI ('MERGINI')
C     !BEG
      call ZEROI (KS,1,6)
      if(HYDR) then
        I = KOOLS
        KS(1) = mod(I,2)
        I = I/2
        KS(2) = mod(I,2)
        I = I/2
        KS(3) = mod(I,2)
        I = I/2
        KS(4) = mod(I,2)
        I = I/2
        KS(5) = mod(I,2)
        KS(6) = KHFFS
C
        do 100 I = 1,6
          KS(I) = min(max(KS(I),0),1)
  100   continue
      end if
C     !END
      call BYE ('MERGINI')
C
      return
      end
