      subroutine ORYZA
     $(ITAU,N,I,J,UU,BDI,GMI,PIJIJ,PIJJI,X,Y)
C
C     Rudolf Loeser, 1968 Mar 25
C---- Computes X and Y, for ZEA.
C     !DASH
      save
C     !DASH
      real*8 BDI, GMI, ONE, PIJIJ, PIJJI, UU, X, Y, ZERO
      integer I, ITAU, J, N
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
      external  ZIZANIA, HI, BYE
C
C               BDI(N,NL), GMI(N,NSL)
      dimension BDI(*),    GMI(*)
C
      call HI ('ORYZA')
C     !BEG
      if(UU.eq.ZERO) then
        X = ONE
        Y = ONE
      else
C
        call ZIZANIA (ITAU,N,BDI,GMI,PIJIJ,PIJJI,UU,X,Y,I,J)
      end if
C     !END
      call BYE ('ORYZA')
C
      return
      end
