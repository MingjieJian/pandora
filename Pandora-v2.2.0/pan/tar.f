      subroutine TAR
     $(EMU,INCI,NW,W,XLTIT,YHZ,YAN,MUX,YY,MYX,KODE,LTYPE,XMULT,LFB,
     $ TAUK,WVNUM,N,LININT)
C
C     Rudolf Loeser, 1983 Nov 04
C---- Writes intensities in special Spectrum Save file, for BENT.
C     (This is version 2 of TAR.)
C     !DASH
      save
C     !DASH
      real*8 EMU, TAUK, W, WVNUM, XLTIT, XMULT, YAN, YHZ, YY
      integer I, INCI, KODE, LFB, LTYPE, LUSO, MUX, MYX, N, NW
      logical LININT
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS(28),LUSO )
C     !DASH
C     !EJECT
      external HI, BYE
C
C               NN = Nmkuse
C
C               LTYPE(NN), XLTIT(NN), XMULT(NN), TAUK(N,NN), WVNUM(NN),
      dimension LTYPE(*),  XLTIT(*),  XMULT(*),  TAUK(N,*),  WVNUM(*),
C
C               YAN(NN), YHZ(NN), MUX(NN), YY(NN), KODE(NN), MYX(NN),
     $          YAN(*),  YHZ(*),  MUX(*),  YY(*),  KODE(*),  MYX(*),
C
C               W(NN)
     $          W(*)
C
      call HI ('TAR')
C     !BEG
      write (LUSO,100) EMU,INCI,LFB,LININT,NW
  100 format('----2  CONTINUUM INTENSITY'/ 1P,
     $       E15.7,2X,'Mu (cosine of look-angle)'/
     $       I10,2X,'INCI =1 if there is incident radiation, ',
     $              '=0 if not'/
     $       I10,2X,'LFB =1 means: front-face, =2 means: back-face'/
     $       L10,2X,'LININT, tells whether this is line-specific ',
     $              'background'/
     $       I10,2X,'# of wavelengths, = # of lines in the ',
     $              'following table:'/
     $       7X,'Each line has: i,WL,WN,I/Hz,I/A,OM,XLTIT,',
     $          '(F KS KI),KODE,LTYPE'/
     $       7X,'I/Hz,I/A,OM,(F KS KI) as in Printout'/
     $       7X,'i = line number'/
     $       7X,'WL = wavelength (Angstroms)'/
     $       7X,'WN = wavenumber'/
     $       7X,'KODE = error code (as printed just to the right ',
     $          'of (F KS KI) if nonzero)'/
     $       7X,'XLTIT,LTYPE = other codes used by PANDORA')
C
      do 102 I = 1,NW
        write (LUSO,101) I,W(I),WVNUM(I),YHZ(I),YAN(I),XMULT(I),
     $                   XLTIT(I),YY(I),MUX(I),MYX(I),KODE(I),LTYPE(I)
  101   format(I10,1P2E20.13,2E13.6,0PF6.3,1PE20.13,0PF7.4,2I4,I3,I10)
  102 continue
C     !END
      call BYE ('TAR')
C
      return
      end
