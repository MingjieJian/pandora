      subroutine PSI
     $(FUL,IA,IB,IC,V)
C
C     Rudolf Loeser, 1992 Jan 21
C---- Computes the function V = Psi(u,l,a,b,c), where FUL is the term
C     depending on the transition indices (u,l), and a, b, and c are
C     integers .ge. 0.
C     !DASH
      save
C     !DASH
      real*8 BA, BB, BC, BJ, F, FA, FB, FC, FJ, FUL, ONE, P, V
      integer IA, IB, IC, J, JJ
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  HI, BYE
      intrinsic min
C
      call HI ('PSI')
C     !BEG
      V = ONE
C
      JJ = min(IB,IC)
      if(JJ.gt.0) then
        P  = ONE
        FB = IB+1
        BB = ONE
        FC = IC+1
        BC = ONE
        FA = IA
        BA = ONE
        BJ = ONE
        do 100 J = 1,JJ
          FB = FB-ONE
          BB = BB*FB
          FC = FC-ONE
          BC = BC*FC
          FA = FA+ONE
          BA = BA*FA
          FJ = J
          BJ = BJ*FJ
          F = (BB/BJ)*(BC/BA)
          P = P*FUL
          V = V+F*P
  100   continue
C
      end if
C     !END
      call BYE ('PSI')
C
      return
      end
