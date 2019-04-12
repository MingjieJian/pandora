      subroutine BENT
     $(INCRAD,J,EMU,NW,WAVES,XIHZ,XIAN,KODE,MUX,YY,MYX,BRIGHT,XIHZA,
     $ XIANA,BRIGHTA,XLTIT,LTYPE,XMLT,LFB,TAUK,WVNUM,N,CONINT,LININT)
C
C     Rudolf Loeser, 1978 May 28
C---- Saves continuum intensities for later uses, for MIXQUIC.
C     !DASH
      save
C     !DASH
      real*8 BRIGHT, BRIGHTA, EMU, TAUK, WAVES, WVNUM, XIAN, XIANA,
     $       XIHZ, XIHZA, XLTIT, XMLT, YY
      integer I, IQCPU, J, JSAV, KODE, LFB, LFBS, LTYPE, MUX, MYX, N,
     $        NW
      logical CONINT, EXPAND, INCRAD, LININT, SPHERE
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
C     !DASH
      external KITTY, TAR, PANSA, HI, BYE
C
C               NN = Nmkuse
C
C               WAVES(NN), XIHZ(NN), BRIGHT(NN), BRIGHTA(NN), TAUK(NN),
      dimension WAVES(*),  XIHZ(*),  BRIGHT(*),  BRIGHTA(*),  TAUK(*),
C
C               MYX(NN), XMLT(NN), XIAN(NN), YY(NN), MUX(NN), KODE(NN),
     $          MYX(*),  XMLT(*),  XIAN(*),  YY(*),  MUX(*),  KODE(*),
C
C               XIHZA(NN), XLTIT(NN), WVNUM(NN), LTYPE(NN), XIANA(NN)
     $          XIHZA(*),  XLTIT(*),  WVNUM(*),  LTYPE(*),  XIANA(*)
C
      data EXPAND,SPHERE /.false., .false./
C     !EJECT
C
      call HI ('BENT')
C     !BEG
      if(IQCPU.gt.0) then
C----   For special Spectrum Save file
        call TAR       (EMU, 0, NW, WAVES, XLTIT, XIHZ , XIAN , MUX,
     $                  YY, MYX, KODE, LTYPE, XMLT, LFB, TAUK, WVNUM,
     $                  N, LININT)
        if(INCRAD) then
          call TAR     (EMU, 1, NW, WAVES, XLTIT, XIHZA, XIANA, MUX,
     $                  YY, MYX, KODE, LTYPE, XMLT, LFB, TAUK, WVNUM,
     $                  N, LININT)
        end if
      end if
C
      call KITTY       (SPHERE, JSAV)
      if((JSAV.gt.0).and.(J.eq.1).and.CONINT) then
C----   For Spectrum Summary file
        LFBS = 100*LFB
        do 100 I = 1,NW
          call PANSA   (WAVES(I), XIHZ(I) , MUX(I), YY(I), MYX(I),
     $                  BRIGHT(I) , XLTIT(I), LFBS    , 2, EXPAND)
          if(INCRAD) then
            call PANSA (WAVES(I), XIHZA(I), MUX(I), YY(I), MYX(I),
     $                  BRIGHTA(I), XLTIT(I), (LFBS+1), 3, EXPAND)
          end if
  100   continue
      end if
C     !END
      call BYE ('BENT')
C
      return
      end
