      subroutine ANIUC
     $(N,NL,VM,VL,VK)
C
C     Rudolf Loeser, 1998 Jul 23
C---- Sets up velocities, for CUNINA.
C     !DASH
      save
C     !DASH
      real*8 VK, VL, VM
      integer J, N, NL
C     !DASH
      external MOVE1, NEGATE, HI, BYE
C
C               VL(N,NL), VK(N), VM(N)
      dimension VL(N,*),  VK(*), VM(*)
C
      call HI ('ANIUC')
C     !BEG
      call MOVE1   (VM,N,VK)
      call NEGATE  (VK,N)
      do 100 J = 1,NL
        call MOVE1 (VM,N,VL(1,J))
  100 continue
C     !END
      call BYE ('ANIUC')
C
      return
      end
