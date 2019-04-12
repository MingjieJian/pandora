      subroutine TOUCAN
     $(LU,J,DDL,N,YHZ,WS,SNU,VEC)
C
C     Rudolf Loeser, 2004 Jun 07
C---- Prints line-core depths-of-formation.
C     (This is version 2 of TOUCAN.)
C     !DASH
      save
C     !DASH
      real*8 DDL, FAC, ONE, SNU, VEC, WS, YHZ
      integer I, J, LU, N
      character LAB*40
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external DIVIDE, VECOUT, HI, BYE
C
C               WS(N), SNU(N), VEC(N)
      dimension WS(*), SNU(*), VEC(*)
C
      call HI ('TOUCAN')
C     !BEG
      if(LU.gt.0) then
        call DIVIDE (ONE, YHZ, FAC)
        do 100 I = 1,N
          VEC(I) = (WS(I)*SNU(I))*FAC
  100   continue
        write (LAB,101) DDL,J
  101   format('***** For DDL =',F9.3,' = DL(',I5,')')
        call VECOUT (LU, VEC, N, LAB)
      end if
C     !END
      call BYE ('TOUCAN')
C
      return
      end
