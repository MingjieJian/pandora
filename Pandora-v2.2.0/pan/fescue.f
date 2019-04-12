      subroutine FESCUE
     $(LU,IU,IL,DLJ,MUSE,DPI,DWI,VI,XNEI,DVI,AI,WVL,DDL,R)
C
C     Rudolf Loeser, 1991 Dec 16
C---- Writes a printout header, for EULE via FASAN.
C     (This is version 3 of FESCUE.)
C     !DASH
      save
C     !DASH
      real*8 AI, DDL, DLJ, DPI, DVI, DWI, R, VI, WVL, XNEI
      integer IL, IU, LU, MUSE
C     !DASH
      external ABJECT, LINER, HI, BYE
C
      call HI ('FESCUE')
C     !BEG
      if(LU.gt.0) then
        call ABJECT (LU)
        write (LU,100)
  100   format(' ','Detailed printout from the calculation of a value ',
     $             'of the line absorption profile.')
        call LINER  (1, LU)
        write (LU,101) IU,IL,WVL,DDL,DLJ,MUSE
  101   format(' ','Line (',I2,'/',I2,'), WVL =',1PE20.12,5X,
     $             'DDL =',E13.5,5X,'DL =',E16.8,5X,'MUSE =',I2)
        call LINER (1, LU)
        write (LU,102) DPI,DWI,VI,DVI,XNEI,AI,R,(DVI-DDL)
  102   format(' ','DP =',1PE16.8,5X,'DW =',E16.8,5X,'V =',E16.8,5X,
     $             'DV =',E16.8,5X,'NE =',E16.8//
     $         ' ','For Voigt function',T25,'A =',E18.8/
     $         ' ',T25,'U = (',E16.8,') * abs( DL +',E16.8,')')
      end if
C     !END
      call BYE ('FESCUE')
C
      return
      end
