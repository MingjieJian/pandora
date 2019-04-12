      subroutine PANICUM
     $(ITAU,IU,IL,N,NL,NLM,KRJ,GMI,RHOIJ,KIJ,YBAR,WEIGHT,Z,X,IX,ZM11)
C
C     Rudolf Loeser, 1968 Mar 26
C---- Computes little-m(1,1), for AVENA.
C                             Not for VAMOS!
C
C     See also PANICKY.
C     !DASH
      save
C     !DASH
      real*8 ARHO, GMI, RHOIJ, SUM, UU, WEIGHT, X, YBAR, Z, ZERO, ZM11
      integer IL, ITAU, IU, IX, KIJ, KRJ, M, N, NL, NLM
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external  ROWSUM, BAMBUSA, ARROW, HI, BYE
C
      dimension X(*), IX(*)
C
C               GMI(N,NSL), YBAR(N,NT), RHOIJ(N,NT), WEIGHT(MUL,NT),
      dimension GMI(N,*),   YBAR(*),    RHOIJ(*),    WEIGHT(*),
C
C               KIJ(NL,NL), Z(NL,NL)
     $          KIJ(*),     Z(NL,*)
C
      call HI ('PANICUM')
C     !BEG
      call ROWSUM      (Z(IU,1), 1, NL, 1, NL, SUM)
      do 101 M = 1,(IU-1)
        if(M.ne.IL) then
          call BAMBUSA (IU, M, IU, IL, WEIGHT, KIJ, NL, UU)
          if(UU.ge.ZERO) then
            call ARROW (ITAU, IU, M, KRJ, RHOIJ, YBAR, X, IX, ARHO)
            SUM = SUM+ARHO
          end if
        end if
  101 continue
      ZM11 = GMI(ITAU,IU)*SUM
C     !END
      call BYE ('PANICUM')
C
      return
      end
