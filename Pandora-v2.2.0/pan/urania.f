      subroutine URANIA
     $(N,HND,RHEAB,HEABD,ASTAR)
C
C     Rudolf Loeser, 1991 Jan 04
C---- Computes a*, a quantity characterizing the mass-motion velocity.
C     !DASH
      save
C     !DASH
      real*8 ASTAR, C, D, DI, HEABD, HND, RHEAB, ZERO
      integer I, II, N
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
C               HND(N), RHEAB(N)
      dimension HND(*), RHEAB(*)
C
      data C /1.D10/
C
      call HI ('URANIA')
C     !BEG
      II = -1
      DI = HND(N)
      do 100 I = 1,N
        D = HND(I)-C
        if(D.gt.ZERO) then
          if(D.lt.DI) then
            II = I
            DI = D
          end if
        end if
  100 continue
      if(II.eq.-1) then
        ASTAR = RHEAB(N)
      else if(II.eq.1) then
        ASTAR = RHEAB(1)
      else
        D = HND(II)-HND(II-1)
        if(D.eq.ZERO) then
          ASTAR = RHEAB(1)
        else
          ASTAR = (RHEAB(II-1)*(HND(II)-C)+RHEAB(II)*(C-HND(II-1)))/D
        end if
      end if
      ASTAR = HEABD*ASTAR
C     !END
      call BYE ('URANIA')
C
      return
      end
