      subroutine PADGE
     $(A,N,MM)
C
C     Rudolf Loeser, 1998 Apr 16
C---- Checks for 0's at the end of A, for diffusion.
C     (This is version 3 of PADGE.)
C     !DASH
      save
C     !DASH
      real*8 A, ZERO
      integer I, MM, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external  HI, BYE
      intrinsic min, max
C
C               A(N)
      dimension A(*)
C
      call HI ('PADGE')
C     !BEG
      MM = N-1
      do 100 I = 1,N
        MM = MM-1
        if(A(MM).ne.ZERO) then
          goto 101
        end if
  100 continue
C
  101 continue
      MM = min(max((MM+1),1),N)
C     !END
      call BYE ('PADGE')
C
      return
      end
