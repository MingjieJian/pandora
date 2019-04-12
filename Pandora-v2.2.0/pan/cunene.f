      subroutine CUNENE
     $(N,XLM,WLIN,XLR,XLTR)
C
C     Rudolf Loeser, 2002 Sep 18
C---- Compute wavelength terms for Hydrogen Lyman N/1 line wing opacity.
C     (This is version 2 of CUNENE.)
C     !DASH
      save
C     !DASH
      real*8 ONE, WLIN, XLM, XLMCR, XLR, XLTR, ZERO
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
      equivalence (RZQ(165),XLMCR)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external RIGEL, HI, BYE
C
      call HI ('CUNENE')
C     !BEG
      XLR  = ((WLIN/XLM)-ONE)**2
C
      if(N.eq.2) then
        XLTR = XLMCR*XLR
      else
        XLTR = ZERO
      end if
C     !END
      call BYE ('CUNENE')
C
      return
      end
