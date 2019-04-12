      subroutine ACORUS
     $(LU,NKA,WAVE)
C
C     Rudolf Loeser, 1988 Dec 06
C---- Prints scattering albedo reference wavelength
C     !DASH
      save
C     !DASH
      real*8 WAVE
      integer LU, NKA
C     !DASH
      external LINER, HI, BYE
C
      call HI ('ACORUS')
C     !BEG
      if((LU.gt.0).and.(NKA.le.0)) then
        call LINER (1,LU)
        write (LU,100) WAVE
  100   format(' ','The Albedo values below are calculated at the ',
     $             'wavelength ',1PE14.6,' Angstroms.')
      end if
C     !END
      call BYE ('ACORUS')
C
      return
      end
