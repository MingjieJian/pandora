      subroutine ZEA
     $(ITAU,IU,IL,N,NL,KRJ,CIJ,PIJ,GMI,BDI,RHOIJ,KIJ,WEIGHT,YBAR,X,IX,Z)
C
C     Rudolf Loeser, 1968 Mar 25
C---- Computes the Z matrix needed for statistical equilibrium
C     calculations.
C                                  > Not for VAMOS!
C
C     See also ZIA & BAKER.
C     !DASH
      save
C     !DASH
      real*8 B, BDI, CADD, CIJ, G, GMI, PIJ, RHOIJ, T, UU, WEIGHT, X,
     $       XX, YBAR, YY, Z, ZERO, ZZ
      integer I, IJ, IL, ITAU, IU, IX, J, JI, KIJ, KRJ, N, NL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external  INDXIJ, BAMBUSA, ORYZA, CADDY, ARROW, DIVIDE, HI, BYE
C
      dimension X(*), IX(*)
C
C               PIJ(N,NL**2), CIJ(N,NL**2), GMI(N,NSL), WEIGHT(MUL,NT),
      dimension PIJ(N,*),     CIJ(N,*),     GMI(N,*),   WEIGHT(*),
C
C               KIJ(NL,NL), BDI(N,NL), RHOIJ(N,NT), YBAR(N,NT), Z(NL,NL)
     $          KIJ(*),     BDI(N,*),  RHOIJ(*),    YBAR(*),    Z(NL,*)
C
      call HI ('ZEA')
C     !BEG
      do 101 J = 1,NL
        do 100 I = 1,NL
          if(I.eq.J) then
            ZZ = ZERO
C
          else
            call INDXIJ   (I, J, IJ)
            call INDXIJ   (J, I, JI)
            call BAMBUSA  (I, J, IU, IL, WEIGHT, KIJ, NL, UU)
            call ORYZA    (ITAU, N, I, J, UU, BDI, GMI, PIJ(ITAU,IJ),
     $                     PIJ(ITAU,JI), XX, YY)
            call CADDY    (ITAU, I, J, KRJ, IU, IL, YBAR, X, IX, CADD)
            ZZ = (CIJ(ITAU,IJ)+CADD)*XX+PIJ(ITAU,IJ)*YY
C
            if((I.le.J).and.(UU.lt.ZERO)) then
              call ARROW  (ITAU, J, I, KRJ, RHOIJ, YBAR, X, IX, T)
              call DIVIDE (BDI(ITAU,J), BDI(ITAU,I), B)
              call DIVIDE (GMI(ITAU,J), GMI(ITAU,I), G)
              ZZ = ZZ-(G*B)*T
C
            end if
          end if
          Z(I,J) = ZZ
C
  100   continue
  101 continue
C     !END
      call BYE ('ZEA')
C
      return
      end
