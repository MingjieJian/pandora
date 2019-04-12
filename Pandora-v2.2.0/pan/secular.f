      subroutine SECULAR
     $(NL,GMI,Z,XR,SR)
C
C     Rudolf Loeser, 2003 Nov 18
C---- Computes the vector R-prime, XR, and
C     the variable script-R, SR, for the VAMOS method.
C
C     See also SECALE.
C     !DASH
      save
C     !DASH
      real*8 GMI, SR, TERM, XR, Z, ZERO
      integer J, NL, NLM
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
C               GMI(NL), Z(NL,NL), XR(NL-1)
      dimension GMI(*),  Z(NL,*),  XR(*)
C
      call HI ('SECULAR')
C     !BEG
      NLM = NL-1
      if(NLM.gt.0) then
C
        SR = ZERO
        do 100 J = 1,NLM
          TERM  = GMI(1)*Z(1,J+1)
          XR(J) = TERM
          SR    = SR+TERM
  100   continue
C
      end if
C     !END
      call BYE ('SECULAR')
C
      return
      end
