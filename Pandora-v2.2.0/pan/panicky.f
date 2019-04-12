      subroutine PANICKY
     $(IU,IL,NL,GMI,SAIJ,YBRIJ,Z,X,IX,ZM11)
C
C     Rudolf Loeser, 2003 Nov 18
C---- Computes little-m(1,1), for the VAMOS method.
C
C     See also PANICUM.
C     !DASH
      save
C     !DASH
      real*8 GMI, Q, SAIJ, SUM, X, YBRIJ, Z, ZM11
      integer IL, IU, IX, M, NL
C     !DASH
      external ROWSUM, CUBIT, HI, BYE
C
      dimension X(*), IX(*)
C
C               GMI(NL), SAIJ(NL,NL), Z(NL,NL), YBRIJ(NL,NL)
      dimension GMI(*),  SAIJ(*),     Z(NL,*),  YBRIJ(*)
C
      call HI ('PANICKY')
C     !BEG
      call ROWSUM    (Z(IU,1), 1, NL, 1, NL, SUM)
C
      do 101 M = 1,(IU-1)
        if(M.ne.IL) then
          call CUBIT (IU, M, IU, IL, NL, SAIJ, YBRIJ, X, IX, Q)
          SUM = SUM+Q
        end if
  101 continue
C
      ZM11 = GMI(IU)*SUM
C     !END
      call BYE ('PANICKY')
C
      return
      end
