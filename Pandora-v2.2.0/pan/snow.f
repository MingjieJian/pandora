      subroutine SNOW
     $(N,MF,ML,IWS,ISIG)
C
C     Rudolf Loeser, 1978 Jul 26
C---- Controls marker settings, for OOBLECK.
C     (This is version 2 of SNOW.)
C     !DASH
      save
C     !DASH
      integer ISIG, IWS, J, MF, ML, N
C     !DASH
      external TAIFUN, HI, BYE
C
C               IWS(N,NW), ISIG(N,NW)
      dimension IWS(N,*),  ISIG(N,*)
C
      call HI ('SNOW')
C     !BEG
      do 100 J = MF,ML
        call TAIFUN (N,IWS(1,J),ISIG(1,J))
  100 continue
C     !END
      call BYE ('SNOW')
C
      return
      end
