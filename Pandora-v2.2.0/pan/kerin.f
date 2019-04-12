      subroutine KERIN
     $(NO,NSW,SCOW)
C
C     Rudolf Loeser, 2002 Aug 16
C---- Prints selected Continuum Output wavelengths for ABAKAN.
C     !DASH
      save
C     !DASH
      real*8 SCOW
      integer NO, NSW
C     !DASH
      external DVECOUT, LINER, HI, BYE
C
C               SCOW(NSW)
      dimension SCOW(*)
C
      call HI ('KERIN')
C     !BEG
      if((NO.gt.0).and.(NSW.gt.0)) then
        call LINER   (2,NO)
        write (NO,100)
  100   format(' ','Selected Wavelengths for Continuum Printout')
C
        call DVECOUT (NO,SCOW,NSW,'SCOW')
      end if
C     !END
      call BYE ('KERIN')
C
      return
      end
