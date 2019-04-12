      subroutine TSUNI
     $(W,IW,ML,XM,XMS,ITAU,LEGEND,KODE)
C
C     Rudolf Loeser, 1981 Feb 13
C---- Inverts XM for basic b-ratios computation.
C     !DASH
      save
C     !DASH
      real*8 W, XM, XMS
      integer ITAU, IW, KODE, LUEO, ML
      character LEGEND*33, TIT*50
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MOTOR, MESHED, DARROUT, MASHED, HI, BYE
C
      dimension W(*), IW(*)
C
C               XM(ML,ML), XMS(ML,ML)
      dimension XM(*),     XMS(*)
C
      call HI ('TSUNI')
C     !BEG
      write (TIT,100) LEGEND,ITAU
  100 format(A33,', at depth #',I3)
C
      call MOTOR     (XM, ML, TIT, W, IW, KODE)
C
      if(KODE.eq.0) then
        call MESHED  ('TSUNI', 1)
        call DARROUT (LUEO, XMS, ML, ML, 'Original Matrix')
        call MASHED  ('TSUNI')
      end if
C     !END
      call BYE ('TSUNI')
C
      return
      end
