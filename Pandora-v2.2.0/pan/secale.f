      subroutine SECALE
     $(ITAU,N,NL,NSL,MSL,KRJ,GMI,XND,NOK,RHOIJ,YBAR,Z,X,IX,XR,SR)
C
C     Rudolf Loeser, 1968 Mar 26
C---- Computes the vector R-prime, XR, and
C     the variable script-R, SR, for GRASS.
C                                > Not for VAMOS.
C
C     See also SECULAR.
C     !DASH
      save
C     !DASH
      real*8 GMI, GMI1, RHOIJ, SLT, SR, SUMZ, X, XND, XR, YBAR, Z, ZERO
      integer ITAU, IX, J, KRJ, MSL, N, NL, NSL
      logical NOK
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external RICE, HI, BYE
C
      dimension X(*), IX(*)
C
C               GMI(N,NSL), Z(NL,NL), XR(NL-1), XND(N,NL), RHOIJ(N,NT),
      dimension GMI(N,*),   Z(NL,*),  XR(*),    XND(*),    RHOIJ(*),
C
C               YBAR(N,NT)
     $          YBAR(*)
C
      call HI ('SECALE')
C     !BEG
      if(NL.gt.1) then
        GMI1 = GMI(ITAU,1)
C
C----   Set up XR (and sum of Z values)
        SUMZ = ZERO
        do 100 J = 1,(NL-1)
          SUMZ  = SUMZ+Z(1,J+1)
          XR(J) = GMI1*Z(1,J+1)
  100   continue
C
        call RICE (ITAU, N, NL, NSL, MSL, KRJ, XND, NOK, RHOIJ, YBAR,
     $             X, IX, SLT)
        SR = GMI1*(SUMZ+SLT)
      end if
C     !END
      call BYE ('SECALE')
C
      return
      end
