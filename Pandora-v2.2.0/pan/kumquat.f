      subroutine KUMQUAT
     $(II,NRY,NRP,IMX,ROW,ADEPTH)
C
C     Rudolf Loeser, 1982 Sep 22
C---- Recovers depth-points-data from ROW and moves it to a row of the
C     matrix ADEPTH, for DEIRA.
C     (This is version 2 of KUMQUAT.)
C     !DASH
      save
C     !DASH
      real*8 ADEPTH, ROW
      integer IA, II, IMN, IMX, NRP, NRY
C     !DASH
      external MOVED, HI, BYE
C
C               ROW(NRP), ADEPTH(NRY,NRY)
      dimension ROW(*),   ADEPTH(NRY,*)
C
      call HI ('KUMQUAT')
C     !BEG
      IMN = NRY-1
C
C---- Set up index IA
      if(II.le.IMN) then
        IA = II
      else if(II.eq.IMX) then
        IA = NRY
      else
        IA = 0
      end if
C
      if(IA.ne.0) then
C----   Move all but last point from ROW into IA'th row of ADEPTH
        call MOVED (ROW,1,IMN,ADEPTH(IA,1),NRY,IMN)
C
C----   Set up last element of that row
        ADEPTH(IA,NRY) = ROW(IMX)
      end if
C     !END
      call BYE ('KUMQUAT')
C
      return
      end
