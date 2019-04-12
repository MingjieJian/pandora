      subroutine RHODES
     $(KCOSCAT,HN,N,RSC)
C
C     Rudolf Loeser, 2004 Aug 31
C---- Computes a term related to CO-lines scattering.
C     (This is version 2 of RHODES.)
C     !DASH
      save
C     !DASH
      real*8 FAC, HN, ONE, RCO, RSC, SRCO
      integer I, KCOSCAT, N
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
      equivalence (RZQ(141),SRCO )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external ONE1, HI, BYE
C
C               HN(N,Limp), RSC(N)
      dimension HN(N,*),    RSC(*)
C
      data FAC /1.D-15/
C
      call HI ('RHODES')
C     !BEG
      if(KCOSCAT.gt.0) then
        do 100 I = 1,N
          RCO    = SRCO*(HN(I,1)*FAC)
          RSC(I) = RCO/(ONE+RCO)
  100   continue
      else
        call ONE1 (RSC, N)
      end if
C     !END
      call BYE ('RHODES')
C
      return
      end
