      subroutine DILLY
     $(F,N,SLOT)
C
C     Rudolf Loeser, 1991 Jun 11
C---- Finds and encodes depth-of-largest-change, for JAY.
C     !DASH
      save
C     !DASH
      real*8 A, B, F, ONE
      integer I, J, N
      character SLOT*5
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
      intrinsic abs
C
C               F(N)
      dimension F(*)
C
      call HI ('DILLY')
C     !BEG
      A = abs(F(1)-ONE)
      J = 1
      do 100 I = 2,N
        B = abs(F(I)-ONE)
        if(B.gt.A) then
          A = B
          J = I
        end if
  100 continue
C
      write (SLOT,101) J
  101 format('[',I3,']')
C     !END
      call BYE ('DILLY')
C
      return
      end
