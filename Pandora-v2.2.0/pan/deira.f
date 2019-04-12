      subroutine DEIRA
     $(ARAY,ADEPTH,NRPMX,I,NRP,IMX,IDM,ROW)
C
C     Rudolf Loeser, 1983 Feb 24
C---- Recovers an array corresponding to depth points from an array
C     corresponding to shell ray points.
C     !DASH
      save
C     !DASH
      real*8 ADEPTH, ARAY, ROW
      integer I, IDM, II, IMX, NRP, NRPMX
C     !DASH
      external MOVED, KUMQUAT, HI, BYE
C
C               ARAY(IDM,NRPMX), ADEPTH(NRP,IMX), ROW(NRPMX)
      dimension ARAY(IDM,*),     ADEPTH(*),       ROW(*)
C
      call HI ('DEIRA')
C     !BEG
C---- Loop over all needed rows
      do 100 II = 1,IMX
C----   Extract II'th row into ROW
        call MOVED   (ARAY(II,1),IDM,NRP,ROW,1,NRP)
C----   Extract needed elements from ROW into ADEPTH
        call KUMQUAT (II,I,NRP,IMX,ROW,ADEPTH)
  100 continue
C     !END
      call BYE ('DEIRA')
C
      return
      end
