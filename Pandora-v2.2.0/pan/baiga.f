      subroutine BAIGA
     $(NO,XLCR,XICR,XYCR,NCR)
C
C     Rudolf Loeser, 1981 May 07
C---- Prints incident coronal radiation tables, for RUPERT.
C     !DASH
      save
C     !DASH
      real*8 XICR, XLCR, XYCR
      integer I, NCR, NO
      character YC*10
C     !DASH
      external LINER, HAKO, SHIM, HI, BYE
C
C               XLCR(NCR), XICR(NCR), XYCR(NCR)
      dimension XLCR(*),   XICR(*),   XYCR(*)
C
      call HI ('BAIGA')
C     !BEG
      write (NO,100)
  100 format(' ',21X,'LCR',17X,'ICR',7X,'YCR')
      call LINER  (1,NO)
C
      do 102 I = 1,NCR
        call HAKO (XYCR(I),YC)
        write (NO,101) I,XLCR(I),XICR(I),YC
  101   format(' ',I4,1P2E20.8,A10)
        call SHIM (I,5,NO)
  102 continue
C     !END
      call BYE ('BAIGA')
C
      return
      end
