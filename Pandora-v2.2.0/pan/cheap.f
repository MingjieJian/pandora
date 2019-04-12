      subroutine CHEAP
     $(LU)
C
C     Rudolf Loeser, 2005 Aug 23
C---- Prints an explanation for rates integration wavelengths.
C     (This is version 2 of CHEAP.)
C     !DASH
      save
C     !DASH
      integer LU
C     !COM
C---- ISOLA       as of 1997 Nov 19
      real*8      WAVEDEL
      common      /ISOLA/ WAVEDEL
C     Two Continuum Wavelength values are "equal" if their
C     relative difference is less than WAVEDEL.
C     .
C     !DASH
      external LINER, HI, BYE
C
      call HI ('CHEAP')
C     !BEG
      if(LU.gt.0) then
        call LINER (2, LU)
        write (LU,100) WAVEDEL
  100   format(' ','(For the rates calculations [and for other ',
     $             'purposes] PANDORA relies on wavelength values ',
     $             '[in Angstroms] that are calculated'/
     $         ' ','as needed. It is important that any two ',
     $             'calculated wavelengths bear their proper '
     $             '[i.e. the intended]'/
     $         ' ','relationship:  .lt.,  .eq., or  .gt.  To insure ',
     $             'that  .eq.  is determined properly, the ',
     $             'relative difference is examined,'/
     $         ' ','and taken to be  .eq. 0  if its ',
     $             'magnitude is less than WAVEDEL =',1PE12.5,' .')
        write (LU,101)
  101   format(' ','To insure that wavelengths computed from ',
     $             'RRNUs at the head of the continuum are ',
     $             'recognized as  .le.  the continuum edge,'/
     $         ' ','such RRNU values are jiggled minutely to be ',
     $             'slightly  .gt. 1.'/
     $         ' ','Also, since wavelengths derived from Lyman-XK ',
     $             'values must not  .eq.  wavelengths derived ',
     $             'from the RRNU(KOLEV) table'/
     $         ' ','[when option LYMAN = on], such RRNU values ',
     $             'are jiggled just a tad more.)')
      end if
C     !END
      call BYE ('CHEAP')
C
      return
      end
