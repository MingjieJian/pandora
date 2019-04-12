      subroutine ANOMURA
     $(LUMR,IU,IL,CRD,FCRD,CVW,FCVW,CSK,FCSK,CRS,FCRS)
C
C     Rudolf Loeser, 1992 Jan 14
C---- Saves atomic transiton data for BURBOT.
C     (This is version 2 of ANOMURA.)
C     !DASH
      save
C     !DASH
      real*8 CRD, CRS, CSK, CVW
      integer IL, IU, LUMR, MODE
      logical FCRD, FCRS, FCSK, FCVW
      character LAB*14
C     !DASH
      external PUNT, HI, BYE
C
      data MODE /1/
C
      call HI ('ANOMURA')
C     !BEG
      write (LAB,100) IU,IL
  100 format(8X,2I3)
C
      if(FCRD) then
        call PUNT (LUMR,CRD,1,MODE,'CRD'//LAB)
      end if
      if(FCVW) then
        call PUNT (LUMR,CVW,1,MODE,'CVW'//LAB)
      end if
      if(FCSK) then
        call PUNT (LUMR,CSK,1,MODE,'CSK'//LAB)
      end if
      if(FCRS) then
        call PUNT (LUMR,CRS,1,MODE,'CRS'//LAB)
      end if
C     !END
      call BYE ('ANOMURA')
C
      return
      end
