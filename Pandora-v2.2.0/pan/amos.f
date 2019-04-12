      subroutine AMOS
     $(IU,IL,TEMP,CE)
C
C     Rudolf Loeser, 2006 Dec 04
C---- Computes default values of CE(IU,IL) for Hydrogen,
C     for 2 .le. IU .le. 5
C
C     Aggarwal, K.M., Berrington, K.A., Burke, P.G.,
C     Kingston. A.E., & Pathak, A. 1991, J.Phys.B., 24, 1385.
C
C     !DASH
      save
C     !DASH
      real*8 CE, CON, FAC, PIL, RT, SIGMA, TEMP, ZERO
      integer IL, IU
      logical INOK, TEOK
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external SOMA, HI, BYE
C
      dimension PIL(4)
C
      data PIL /2.D0, 8.D0, 18.D0, 32.D0/
      data CON /8.629D-6/
C
      call HI ('AMOS')
C     !BEG
      CE = ZERO
C
      INOK = (IU.ge.2).and.(IU.le.5).and.(IL.lt.IU).and.(IL.gt.0)
      TEOK = TEMP.gt.ZERO
      if(INOK.and.TEOK) then
        call SOMA (IU, IL, TEMP, SIGMA)
        RT  = sqrt(TEMP)
        FAC = CON/(PIL(IL)*RT)
C
        CE = FAC*SIGMA
      end if
C     !END
      call BYE ('AMOS')
C
      return
      end
