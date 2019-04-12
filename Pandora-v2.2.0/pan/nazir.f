      subroutine NAZIR
     $(LU,KAMB,KVLG)
C
C     Rudolf Loeser, 1989 Oct 17
C---- Prints heading for diffusion calculations.
C     !DASH
      save
C     !DASH
      integer INDX, IQA1D, IQA1P, IQADP, IQAMB, IQAMD, IQAN1, IQDSM,
     $        IQGNV, IQHEA, IQHNM, IQNRS, IQVDP, IQVLG, IQVLP, KAMB,
     $        KVLG, LU, jummy
      character A1D*3, A1P*3, ADP*3, AMB*3, AMD*3, AN1*3, DSM*3, GNV*3,
     $          HEA*3, HNM*3, LAB*4, NRS*3, VDP*3, VLG*3, VLP*3
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
      equivalence (IQQ(266),IQDSM)
      equivalence (IQQ(244),IQGNV)
      equivalence (IQQ(219),IQAMD)
      equivalence (IQQ(221),IQVLG)
      equivalence (IQQ(264),IQAMB)
      equivalence (IQQ(220),IQADP)
      equivalence (IQQ(222),IQVDP)
      equivalence (IQQ(272),IQAN1)
      equivalence (IQQ(281),IQHEA)
      equivalence (IQQ(319),IQNRS)
      equivalence (IQQ(239),IQVLP)
      equivalence (IQQ(267),IQA1P)
      equivalence (IQQ(268),IQA1D)
      equivalence (IQQ(230),IQHNM)
C     !DASH
C     !EJECT
      external  PRIAM, ONOFF, LINER, HI, BYE
      intrinsic max
C
      dimension LAB(3)
C
      data      LAB /'H', 'HeI', 'HeII'/
C
      call HI ('NAZIR')
C     !BEG
      if(LU.gt.0) then
        call PRIAM  (LU,'DIFFUSION',9)
        call LINER  (2,LU)
        INDX = max(KAMB,KVLG)
        write (LU,100) LAB(INDX),KAMB,KVLG
  100   format(' ','Diffusion and Mass Motion Terms Calculation, ',
     $             'for ',A,10X,'(KAMB =',I2,', KVLG =',I2,')')
C
        call ONOFF  (IQAMD,jummy,AMD)
        call ONOFF  (IQAN1,jummy,AN1)
        call ONOFF  (IQVLG,jummy,VLG)
        call ONOFF  (IQHEA,jummy,HEA)
        call ONOFF  (IQNRS,jummy,NRS)
        call ONOFF  (IQAMB,jummy,AMB)
        call ONOFF  (IQADP,jummy,ADP)
        call ONOFF  (IQVDP,jummy,VDP)
        call ONOFF  (IQVLP,jummy,VLP)
        call ONOFF  (IQHNM,jummy,HNM)
        call ONOFF  (IQDSM,jummy,DSM)
        call ONOFF  (IQGNV,jummy,GNV)
        call ONOFF  (IQA1P,jummy,A1P)
        call ONOFF  (IQA1D,jummy,A1D)
C
        call LINER  (1,LU)
        write (LU,101) 'AMDIFF',AMD,'AMBPRNT',AMB,'AMDDMP',ADP,
     $                 'VELGRAD',VLG,'VLGPRNT',VLP,'VELGDMP',VDP,
     $                 'AMDN1',AN1,'ADN1PRNT',A1P,'ADN1DMP',A1D,
     $                 'GNVCALC',GNV,
     $                 'HENORM',HNM,'HEABD',HEA,'DSMOOTH',DSM,
     $                 'NRSMOOTH',NRS
  101   format(' ','For a complete list and explanation of control ',
     $             'parameters, see Section INPUT.'//
     $         ' ','Summary of status of relevant program options:'//
     $         ' ',6(4X,A10,1X,A3)/
     $         ' ',4(4X,A10,1X,A3)/
     $         ' ',4(4X,A10,1X,A3))
      end if
C     !END
      call BYE ('NAZIR')
C
      return
      end
