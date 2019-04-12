      subroutine HYDRE
     $(ZGM,DGMZ,NGM,Z,DGM,N,IDGMZ)
C
C     Rudolf Loeser, 2006 May 10
C---- Provides default values of DGM.
C     (This is version 2 of HYDRE.)
C     !DASH
      save
C     !DASH
      real*8 DGM, DGMZ, Z, ZGM
      integer IDGMZ, KONST, N, NGM
      logical ZDGM
C     !DASH
      external NAUGHTD, DERE, ONE1, HI, BYE
C
C               ZGM(NGM), DGMZ(NGM), Z(N), DGM(N)
      dimension ZGM(*),   DGMZ(*),   Z(*), DGM(*)
C
      data KONST /1/
C
      call HI ('HYDRE')
C     !BEG
      IDGMZ = 0
      call NAUGHTD  (DGM, 1, NGM, ZDGM)
C
      if(ZDGM) then
        if(NGM.gt.0) then
          call DERE (ZGM, 1, DGMZ, 1, NGM, Z, 1, DGM, 1, N, KONST)
          IDGMZ = 1
        else
          call ONE1 (DGM, N)
        end if
      end if
C     !END
      call BYE ('HYDRE')
C
      return
      end
