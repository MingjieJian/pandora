      subroutine DAFT
     $(XLM,X,F,N,TERM)
C
C     Rudolf Loeser, 1973 Oct 26
C---- Interpolates in Dust tables.
C     !DASH
      save
C     !DASH
      real*8 F, ONE, TERM, X, XLM
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
      call HI ('DAFT')
C     !BEG
      if((XLM.le.X(1)).or.(N.le.1)) then
        TERM = F(1)
      else if(XLM.ge.X(N)) then
        TERM = F(N)
      else
        call NOTMORE (X,N,XLM,I)
        TERM = ((ONE/X(I)-ONE/XLM)*F(I+1)+(ONE/XLM-ONE/X(I+1))*F(I))
     $          /(ONE/X(I)-ONE/X(I+1))
      end if
C     !END
      call BYE ('DAFT')
C
      return
      end
