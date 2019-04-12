      subroutine ROBATS
     $(M,Z,F,G,R,S,Y,CHK,KZANX,W,IW,DUMP)
C
C     Rudolf Loeser, 1998 Apr 21
C---- Does checking, for Special N1 of diffusion calculations.
C     !DASH
      save
C     !DASH
      real*8 CHK, F, G, R, S, W, Y, Z
      integer IDGZ, IDYZ, IGG, IN, IS, IW, IZCM, KZANX, M, MOX
      logical DUMP
C     !DASH
      external STABOR, BOTARS, TOBARS, MOVE1, CATRIN, WGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               J = N+MXTAP
C
C               Z(J), F(J), G(J), R(J), S(J), Y(J), CHK(J), KZANX(N)
      dimension Z(*), F(*), G(*), R(*), S(*), Y(*), CHK(*), KZANX(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),IDYZ  ),(IN( 2),IGG   ),(IN( 3),IDGZ  ),(IN( 4),IZCM  )
C
      call HI ('ROBATS')
C     !BEG
C     (Get, and allocate, W allotment)
      call STABOR   (IN, IS, MOX, 'ROBATS', M)
C
C---- Do calculations
      call MOVE1    (Z, M, W(IZCM))
      call CATRIN   (W(IZCM), M)
      call BOTARS   (M, W(IZCM), F, G, R, S, Y, W(IDYZ), W(IGG),
     $               W(IDGZ), CHK, W, IW)
      if(DUMP) then
        call TOBARS (M, W(IZCM), KZANX, Y, W(IDYZ), W(IGG), W(IDGZ), S,
     $               CHK)
      end if
C
C     (Give back W allotment)
      call WGIVE    (W, 'ROBATS')
C     !END
      call BYE ('ROBATS')
C
      return
      end
