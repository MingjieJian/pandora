      subroutine MUNGA
     $(N,NVEL,VEL,V,USE)
C
C     Rudolf Loeser, 1990 May 18
C---- Checks whether V is equal to any of the sets in VEL.
C     Returns USE = .true. if not.
C     !DASH
      save
C     !DASH
      real*8 V, VEL, ZERO
      integer J, KEQ, N, NVEL
      logical USE
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external ARISOD, HI, BYE
C
C               VEL(N,NVEL), V(N)
      dimension VEL(N,*),    V(*)
C
      call HI ('MUNGA')
C     !BEG
      USE = .true.
C
      do 100 J = 1,NVEL
        call ARISOD (V,N,VEL(1,J),N,ZERO,KEQ)
        if(KEQ.eq.1) then
          USE = .false.
          go to 101
        end if
  100 continue
C
  101 continue
C     !END
      call BYE ('MUNGA')
C
      return
      end
