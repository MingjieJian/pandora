      subroutine YUROK
     $(ML,NL,ITAU,N,XM,KMAT,XR,BDIJ,JNEG,RACK)
C
C     Rudolf Loeser, 1981 Feb 13
C---- Computes basic b-ratios, BDIJ, at depth ITAU.
C
C     Upon return, JNEG .eq. 0 indicates that all seems OK;
C     JNEG .gt. 0 indicates that one or more negative values
C     turned up, the first of which is then returned in RACK.
C     !DASH
      save
C     !DASH
      real*8 BDIJ, ONE, RACK, SUM, XM, XR, ZERO
      integer ITAU, J, JNEG, K, KMAT, ML, N, NL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external SETD, HI, BYE
C
C               XM(ML,ML), XR(ML), BDIJ(N,NL)
      dimension XM(ML,*),  XR(*),  BDIJ(N,*)
C
      call HI ('YUROK')
C     !BEG
      JNEG = 0
      RACK = ONE
C
      if(KMAT.eq.0) then
        call SETD (BDIJ(ITAU,1), N, NL, ONE)
      else
C
        BDIJ(ITAU,1) = ONE
        do 101 J = 1,ML
          SUM = ZERO
          do 100 K = 1,ML
            SUM = SUM+XM(J,K)*XR(K)
  100     continue
          BDIJ(ITAU,J+1) = SUM
          if((SUM.le.ZERO).and.(JNEG.eq.0)) then
            JNEG = J+1
            RACK = SUM
          end if
  101   continue
      end if
C     !END
      call BYE ('YUROK')
C
      return
      end
