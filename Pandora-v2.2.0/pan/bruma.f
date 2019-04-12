      subroutine BRUMA
     $(DELTA,MN1,LIM)
C
C     Rudolf Loeser, 1998 Oct 28
C---- Looks for zeroes at the end of the DELTA table.
C     !DASH
      save
C     !DASH
      real*8 DELTA, ZERO
      integer I, J, LIM, MN1
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external HI, BYE
C
C               DELTA(7,N)
      dimension DELTA(7,*)
C
      call HI ('BRUMA')
C     !BEG
      LIM = MN1
      do 101 J = MN1,1,-1
C
        do 100 I = 1,7
          if(DELTA(I,J).ne.ZERO) goto 102
  100   continue
        LIM = LIM-1
C
  101 continue
C
  102 continue
C     !END
      call BYE ('BRUMA')
C
      return
      end
