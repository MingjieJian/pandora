      subroutine HARLECH
     $(N,XRABD,WRABD)
C
C     Rudolf Loeser, 1998 Mar 27
C---- Sets up Hydrogen "RABD", for diffusion.
C     !DASH
      save
C     !DASH
      real*8 WRABD, XRABD
      integer ION, N
C     !DASH
      external PENDA, MOVE1, ONE1, HI, BYE
C
C               XRABD(N), WRABD(N)
      dimension XRABD(*), WRABD(*)
C
      call HI ('HARLECH')
C     !BEG
      call PENDA   (ION)
      if(ION.eq.1) then
        call MOVE1 (XRABD,N,WRABD)
      else
        call ONE1  (WRABD,N)
      end if
C     !END
      call BYE ('HARLECH')
C
      return
      end
