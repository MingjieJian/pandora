      subroutine JEST
     $(RK,CK,GNV,KDGV,TRM)
C
C     Rudolf Loeser, 2004 Feb 27
C---- Computes a term for PIJ.
C     !DASH
      save
C     !DASH
      real*8 CK, GNV, RK, TRM, ZERO
      integer IPIJG, KDGV
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
      equivalence (KZQ(206),IPIJG)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external HI, BYE
C
      call HI ('JEST')
C     !BEG
      TRM = RK+CK
C
      if((KDGV.ne.0).and.(GNV.ne.ZERO)) then
        TRM = TRM+GNV
C
        if((TRM.lt.ZERO).and.(IPIJG.gt.0)) then
          TRM = CK
        end if
C
      end if
C     !END
      call BYE ('JEST')
C
      return
      end
