      subroutine DURANKI
     $(S,RHO,XJBAR,N)
C
C     Rudolf Loeser, 1981 Apr 21
C---- Computes Jbar.
C     !DASH
      save
C     !DASH
      real*8 ONE, RHO, S, XJBAR
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
      external HI, BYE
C
C               S(N), RHO(N), XJBAR(N)
      dimension S(*), RHO(*), XJBAR(*)
C
      call HI ('DURANKI')
C     !BEG
      do 100 I = 1,N
        XJBAR(I) = S(I)*(ONE-RHO(I))
  100 continue
C     !END
      call BYE ('DURANKI')
C
      return
      end
