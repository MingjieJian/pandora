      subroutine MELVIN
     $(D,C,N)
C
C     Rudolf Loeser, 1992 Apr 09
C---- Eliminates null components for TABOR.
C     !DASH
      save
C     !DASH
      real*8 C, D, ZERO
      integer I, M, N
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
C               D(N), C(N)
      dimension D(*), C(*)
C
      call HI ('MELVIN')
C     !BEG
      M = N
      N = 0
      do 100 I = 1,M
C
        if(C(I).ne.ZERO) then
          N = N+1
          if(N.lt.I) then
            D(N) = D(I)
            C(N) = C(I)
          end if
        end if
C
  100 continue
C     !END
      call BYE ('MELVIN')
C
      return
      end
