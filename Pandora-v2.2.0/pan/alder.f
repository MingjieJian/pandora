      subroutine ALDER
     $(WN,N,IS)
C
C     Rudolf Loeser, 1968 Jun 06 (revised 2000 Jan 19)
C---- Takes care of the upper left corner of the expanded WN matrix.
C     !DASH
      save
C     !DASH
      real*8 ONE, TERM, WN
      integer I, IS, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external MOVED, HI, BYE
C
C               WN(N,N)
      dimension WN(N,*)
C
      call HI ('ALDER')
C     !BEG
      TERM = WN(1,1)+ONE
C
      do 100 I = 2,IS
        call MOVED (WN(1,1),N,N,WN(I,1),N,N)
        WN(I,I) = -ONE
        WN(I,1) = TERM
  100 continue
C     !END
      call BYE ('ALDER')
C
      return
      end
