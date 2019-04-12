      subroutine LIMES
     $(A,N,IL,IG)
C
C     Rudolf Loeser, 1979 Dec 02
C---- Finds indices bracketing a negative interval, for SENTA
C     IL  is the index of the last preceding value .gt. 0,
C     IG  is the index of the first succeeding value .gt. 0.
C     !DASH
      save
C     !DASH
      real*8 A, ZERO
      integer IG, IL, J, JS, N
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
C               A(N)
      dimension A(*)
C     !EJECT
C
      call HI ('LIMES')
C     !BEG
      IL = IG
      IG = N+1
C
      JS = IL+1
      if(JS.gt.N) then
        goto 103
      end if
      do 100 J = JS,N
        if(A(J).le.ZERO) then
          goto 101
        end if
        IL = J
  100 continue
      goto 103
C
  101 continue
      IG = IL+2
      JS = IG
      if(JS.gt.N) then
        goto 103
      end if
      do 102 J = JS,N
        IG = J
        if(A(J).gt.ZERO) then
          goto 103
        end if
  102 continue
      IG = N+1
C
  103 continue
C     !END
      call BYE ('LIMES')
C
      return
      end
