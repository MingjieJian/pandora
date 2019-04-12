      subroutine GET_DATE
     $(DAIT)
C     Rudolf Loeser, 1998 Apr 02
C---- Returns the current calendar date.
C     Format: "YYYY-MMM-DD"
C     !DASH
      save
C     !DASH
      integer DATE, iYY, iMM, iDD
      logical error
      character BLANK*1, DAIT*11, ZERO*1
C     !DASH
      external  idate, MONTH
C
      dimension DATE(3)
      data      BLANK, ZERO /' ', '0'/
C
C     !BEG
      call idate (DATE)
      iYY = date(3)
#ifdef __INTEL_COMPILER
      iMM = date(2)
      iDD = date(1)
#else
      iMM = date(1)
      iDD = date(2)
#endif
      if (iYY.lt.100) iYY = iYY+2000
      write (DAIT,100) iYY, iDD
  100 format(I4,'-',3X,'-',I2)
      call MONTH (2,DAIT(6:8),iMM,error)
      if (error) print *, 'GET_DATE: MONTH() returned error'
      if(DAIT(10:10).eq.BLANK) then
        DAIT(10:10) = ZERO
      end if
C     !END
C
      return
      end
