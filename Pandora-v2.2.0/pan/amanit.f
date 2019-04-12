      subroutine AMANIT
     $(I,N,JS,JE,GM,RL,CK,SQR,SQC)
C
C     Rudolf Loeser, 1978 May 04
C---- Computes sums, for RAGOUT.
C     !DASH
      save
C     !DASH
      real*8 CK, GM, RL, SQC, SQR, ZERO
      integer I, JE, JS, LIM, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external SUMPROD, HI, BYE
C
C               GM(N,NSL), RL(N,NSL), CK(N,NSL)
      dimension GM(N,*),   RL(N,*),   CK(N,*)
C
      call HI ('AMANIT')
C     !BEG
      LIM = JE-JS+1
      if(LIM.gt.0) then
        call SUMPROD (SQR,GM(I,JS),N,RL(I,JS),N,LIM)
        call SUMPROD (SQC,GM(I,JS),N,CK(I,JS),N,LIM)
      else
        SQR = ZERO
        SQC = ZERO
      end if
C     !END
      call BYE ('AMANIT')
C
      return
      end
