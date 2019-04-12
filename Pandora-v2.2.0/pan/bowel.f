      subroutine BOWEL
     $(NO,J,NB,FROM,TO,WAVENO,RFROM,RTO,YHZ,YAN,BT)
C
C     Rudolf Loeser, 1983 Oct 28
C---- Prints, for HEART.
C     !DASH
      save
C     !DASH
      real*8 BT, FROM, RFROM, RTO, TO, YAN, YHZ
      integer J, NB, NO
      logical WAVENO
C     !DASH
      external HI, BYE
C
      call HI ('BOWEL')
C     !BEG
      if(NO.gt.0) then
        if(WAVENO) then
          write (NO,100) J,NB,RFROM,RTO,YHZ,YAN,BT
  100     format(' ',13X,I2,6X,I5,3X,1P2E17.9,3E14.3)
        else
          write (NO,100) J,NB,FROM ,TO ,YHZ,YAN,BT
        end if
      end if
C     !END
      call BYE ('BOWEL')
C
      return
      end
