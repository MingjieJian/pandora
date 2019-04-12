      subroutine RING
     $(N,XLM,LU,IU,IL,FMULT,CAPPA,SIGMA,SCAT,OPAC,BHS,XJNU,CSF)
C
C     Rudolf Loeser, 2006 Feb 09
C---- Prints summary of background (continuum) data for line centers.
C     (This is version 2 of RING.)
C     !DASH
      save
C     !DASH
      real*8 BHS, CAPPA, CSF, FMULT, OPAC, SCAT, SIGMA, XJNU, XLM
      integer I, IL, IU, LU, N, NC
      character LINE*12
C     !DASH
      external LOTHAR, PRIAM, LINER, HI, BYE
C
C               CAPPA(N), SIGMA(N), SCAT(N), OPAC(N), XJNU(N), BHS(N),
      dimension CAPPA(*), SIGMA(*), SCAT(*), OPAC(*), XJNU(*), BHS(*),
C
C               CSF(N)
     $          CSF(*)
C
      call HI ('RING')
C     !BEG
      if(LU.gt.0) then
        call LOTHAR (IU, IL, 1, LINE, NC)
        call PRIAM  (LU, LINE, NC)
        call LINER  (1, LU)
        write (LU,100) IU,IL,XLM,FMULT
  100   format(' ','Summary of background results for center of ',
     $             'Line (',I2,'/',I2,') at =',1PE16.8,' Angstroms.',
     $             13X,'(Option LBDPRNT)'/
     $         ' ','(Opacity multiplier =',0PF10.5,')'///
     $         ' ',10X,'ABS  = background opacity without scattering'/
     $         ' ',10X,'SCAT = background scattering opacity'/
     $         ' ',10X,'r    = scattering ratio'/
     $         ' ',10X,'XPC  = ABS + SCAT'/
     $         ' ',10X,'SABS = background source function without ',
     $             'scattering'/
     $         ' ',10X,'JNU  = mean intensity'/
     $         ' ',10X,'CSF  = background-continuum source function'///
     $         ' ',18X,'ABS',12X,'SCAT',15X,'r',13X,'KPC',12X,'SABS',
     $             13X,'JNU',13X,'CSF')
        call LINER  (1, LU)
        write (LU,101) (I,(FMULT*CAPPA(I)),SIGMA(I),SCAT(I),OPAC(I),
     $                    BHS(I),XJNU(I),CSF(I),I=1,N)
  101   format(5(' ',I5,1P7E16.8/))
      end if
C     !END
      call BYE ('RING')
C
      return
      end
