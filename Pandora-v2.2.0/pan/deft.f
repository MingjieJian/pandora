      subroutine DEFT
     $(XLM,X,F,N,TERM)
C
C     Rudolf Loeser, 2001 Jan 14
C---- Interpolates in Dust tables.
C     !DASH
      save
C     !DASH
      real*8 F, FRQ, ONE, TERM, X, XLM
      integer I, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external NOTMORE, HI, BYE
C
C               X(N), F(N)
      dimension X(*), F(*)
C
      call HI ('DEFT')
C     !BEG
      FRQ = ONE/XLM
      if((FRQ.le.X(1)).or.(N.le.1)) then
        TERM = F(1)
      else if(FRQ.ge.X(N)) then
        TERM = F(N)
      else
        call NOTMORE (X,N,FRQ,I)
        TERM = ((FRQ-X(I))*F(I+1)+(X(I+1)-FRQ)*F(I))/(X(I+1)-X(I))
      end if
C     !END
      call BYE ('DEFT')
C
      return
      end
