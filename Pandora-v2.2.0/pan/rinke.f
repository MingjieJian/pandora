      subroutine RINKE
     $(NO,NDV,DWAVE)
C
C     Rudolf Loeser, 2002 Sep 23
C---- Prints Continuum Calculation dump wavelengths for ABAKAN.
C     !DASH
      save
C     !DASH
      real*8 DWAVE
      integer NDV, NO
C     !DASH
      external DVECOUT, LINER, HI, BYE
C
C               DWAVE(NDW)
      dimension DWAVE(*)
C
      call HI ('RINKE')
C     !BEG
      if((NO.gt.0).and.(NDV.gt.0)) then
        call LINER   (2,NO)
        write (NO,100)
  100   format(' ','Wavelengths for Continuum Calculation dump output')
C
        call DVECOUT (NO,DWAVE,NDV,'DWAVE')
      end if
C     !END
      call BYE ('RINKE')
C
      return
      end
