      subroutine BOTARS
     $(M,ZCM,F,G,R,S,Y,DYZ,GG,DGZ,CHK,W,IW)
C
C     Rudolf Loeser, 1998 Apr 21
C---- Calculates a check quantity, for diffusion.
C     !DASH
      save
C     !DASH
      real*8 CHK, DGZ, DYZ, F, G, GG, R, S, W, Y, ZCM
      integer I, IW, M
C     !DASH
      external PACIFIC, HI, BYE
C
      dimension W(*), IW(*)
C
C               ZCM(N), DYZ(N), DGZ(N), R(N), Y(N), F(N), G(N), CHK(N),
      dimension ZCM(*), DYZ(*), DGZ(*), R(*), Y(*), F(*), G(*), CHK(*),
C
C               GG(N), S(N)
     $          GG(*), S(*)
C
      call HI ('BOTARS')
C     !BEG
C---- Get dy/dZ
      call PACIFIC (ZCM,Y,DYZ,M,W,IW)
C
C---- Get dG/dZ
      do 100 I = 1,M
        GG(I) = G(I)*Y(I)-F(I)*DYZ(I)
  100 continue
      call PACIFIC (ZCM,GG,DGZ,M,W,IW)
C
C---- Get check
      do 101 I = 1,M
        CHK(I) = DGZ(I)+R(I)*Y(I)
  101 continue
C     !END
      call BYE ('BOTARS')
C
      return
      end
