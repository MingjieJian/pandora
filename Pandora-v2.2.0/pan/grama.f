      subroutine GRAMA
     $(N,ITMX,A,PRNT)
C
C     Rudolf Loeser, 1984 Jan 25
C---- Tests whether a set of iterative data is worth printing.
C     Returns PRNT=.true. if yes, =.false. if no.
C     (This is version 2 of GRAMA.)
C     !DASH
      save
C     !DASH
      real*8 A, DELTA, ONE
      integer ITMX, J, KEQ, KNT, LIM, N, NIM
      logical PRNT
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  RANGED, HI, BYE
C
C               A(N,ITMX)
      dimension A(N,*)
C
      data      DELTA /1.D-3/
C
      call HI ('GRAMA')
C     !BEG
      LIM = ITMX-1
      NIM = N*LIM
      KNT = 0
C
      do 100 J=1,LIM
        call RANGED (A(1,J),1,N,DELTA,ONE,KEQ)
        KNT = KNT+KEQ
  100 continue
C
      PRNT = (NIM-KNT).gt.(NIM/10)
C     !END
      call BYE ('GRAMA')
C
      return
      end
