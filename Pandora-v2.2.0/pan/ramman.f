      subroutine RAMMAN
     $(NO,TDUST,XLMD,DFD,ALD,EPD,LDU)
C
C     Rudolf Loeser, 1981 May 07
C---- Prints Type-1 Dust Calculation parameters.
C     (This is version 2 of RAMMAN.)
C     !DASH
      save
C     !DASH
      real*8 ALD, DFD, EPD, TDUST, XLMD
      integer I, LDU, NO
C     !DASH
      external LINER, HI, BYE
C
C               XLMD(LDU), DFD(LDU), ALD(LDU), EPD(LDU)
      dimension XLMD(*),   DFD(*),   ALD(*),   EPD(*)
C
      call HI ('RAMMAN')
C     !BEG
      if(NO.gt.0) then
        write (NO,100) TDUST
  100   format(' ','Constant Temperature TDUST =',1PE13.6//
     $         ' ',17X,'LM',13X,'DF',12X,'ALB',12X,'EPD')
        call LINER (1,NO)
C
        write (NO,101) (I,XLMD(I),DFD(I),ALD(I),EPD(I),I=1,LDU)
  101   format(5(' ',I2,1PE17.8,3E15.4/))
      end if
C     !END
      call BYE ('RAMMAN')
C
      return
      end
