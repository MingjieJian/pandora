      subroutine BAKOTO
     $(W,JJ,N,EM,BF,SN,IMG,S,EDITS)
C
C     Rudolf Loeser, 1981 Apr 21
C           revised, 2004 May 07
C
C---- Computes S for LINDEN.
C     !DASH
      save
C     !DASH
      real*8 BF, EM, S, SN, SUM, W, ZERO
      integer I, IMG, J, JJ, N
      logical EDITS, SOFN
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external EDDIE, HI, BYE
C
      dimension W(*)
C
C               EM(N,N), BF(N), S(N), SN(N), IMG(N)
      dimension EM(N,*), BF(*), S(*), SN(*), IMG(*)
C
      call HI ('BAKOTO')
C     !BEG
C---- Compute (all or part of) S
      do 101 I = 1,N
        SUM = ZERO
        if(I.le.JJ) then
          do 100 J = 1,N
            SUM = SUM+EM(I,J)*BF(J)
  100     continue
        end if
        S(I) = SUM
  101 continue
C
C---- Edit (if requested)
      call EDDIE (S, SN, JJ, IMG, EDITS, SOFN, W)
C     !END
      call BYE ('BAKOTO')
C
      return
      end
