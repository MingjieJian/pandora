      subroutine RAGGED
     $(Z,N,KZAUG,KZANX,ZA,M)
C
C     Rudolf Loeser, 2004 Jan 30
C---- Constructs expanded Z-scale, for KANTOR.
C     !DASH
      save
C     !DASH
      real*8 Z, ZA
      integer I, KZANX, KZAUG, M, N
C     !DASH
      external RAMMED, HI, BYE
C
C               Z(N), KZAUG(N), KZANX(N), ZA(N+MXTAP)
      dimension Z(*), KZAUG(*), KZANX(*), ZA(*)
C
      call HI ('RAGGED')
C     !BEG
      M = 0
      do 100 I = 1,(N-1)
        M = M+1
        ZA(M)    = Z(I)
        KZANX(I) = M
        if(KZAUG(I).gt.0) then
          call RAMMED (KZAUG(I),Z(I),Z(I+1),M,ZA)
        end if
  100 continue
      M = M+1
      ZA(M)    = Z(N)
      KZANX(N) = M
C     !END
      call BYE ('RAGGED')
C
      return
      end
