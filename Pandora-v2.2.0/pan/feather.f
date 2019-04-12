      subroutine FEATHER
     $(NW,W,TF,SF,XMULT,LTYPE,LFB,WVNUM,LINFLX)
C
C     Rudolf Loeser, 1973 Oct 11
C---- Writes out fluxes in special Spectrum Save file, for APEX.
C     !DASH
      save
C     !DASH
      real*8 SF, TF, W, WVNUM, XMULT
      integer I, IQCPU, LFB, LTYPE, LUSO, NW
      logical LINFLX
C     !COM
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ( 97),IQCPU)
C
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
C               WVNUM(Nmkuse), LTYPE(Nmkuse), W(Nmkuse), XMULT(Nmkuse),
      dimension WVNUM(*),      LTYPE(*),      W(*),      XMULT(*),
C
C               TF(Nmkuse), SF(Nmkuse)
     $          TF(*),      SF(*)
C
      call HI ('FEATHER')
C     !BEG
      if(IQCPU.gt.0) then
        write (LUSO,100) LFB,LINFLX,NW
  100   format('----3  CONTINUUM FLUX'/
     $         I10,2X,'LFB =1 means: front-face, =2 means: back-face'/
     $         L10,2X,'LINFLX, tells whether this is line-specific ',
     $                'background'/
     $         I10,2X,'# of wavelengths, = # of lines in the ',
     $                'following table:'/
     $         7X,'Each line has: i,WL,WN,TF,SF,OM,LTYPE'/
     $         7X,'i = line number'/
     $         7X,'WL = wavelength (Angstroms)'/
     $         7X,'WN = wavenumber'/
     $         7X,'TF = total flux, = F/Hz as printed but without ',
     $            'the factor 4 * Pi'/
     $         7X,'SF = shell flux (part of TF)'/
     $         7X,'OM as in Printout'/
     $         7X,'LTYPE = code used by PANDORA')
C
        do 102 I = 1,NW
          write (LUSO,101) I,W(I),WVNUM(I),TF(I),SF(I),XMULT(I),LTYPE(I)
  101     format(I10,1P4E20.12,E12.5,I10)
  102   continue
      end if
C     !END
      call BYE ('FEATHER')
C
      return
      end
