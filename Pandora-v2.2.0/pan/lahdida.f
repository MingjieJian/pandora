      subroutine LAHDIDA
     $(J,I,NL,X,YBRIJ,AIJ,P,T)
C
C     Rudolf Loeser, 2003 Nov 25
C---- Computes the "single-rate T" for the VAMOS method.
C     !DASH
      save
C     !DASH
      real*8 AIJ, P, PRAT, T, X, YARAT, YBRIJ
      integer I, J, NL
C     !DASH
      external RIATA, DIVIDE, HI, BYE
C
      dimension X(*)
C
C               YBRIJ(NL,NL), AIJ(NL,NL), P(NSL)
      dimension YBRIJ(*),     AIJ(NL,*),  P(*)
C
      call HI ('LAHDIDA')
C     !BEG
      call RIATA  (X, J, I, NL, YBRIJ, YARAT)
      call DIVIDE (P(J), P(I), PRAT)
      T = AIJ(J,I)*PRAT*YARAT
C     !END
      call BYE ('LAHDIDA')
C
      return
      end
