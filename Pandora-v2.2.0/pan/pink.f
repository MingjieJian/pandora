      subroutine PINK
     $(WTAB,EM,YNT,MUX,MYX,YY,KODE,TAU,TMU,SNU,WS,N,LFB,LININT)
C
C     Rudolf Loeser, 1970 Feb 05
C---- Intensity dump routine.
C     !DASH
      save
C     !DASH
      real*8 EM, SNU, TAU, TMU, WS, WSNU, WTAB, YNT, YY
      integer I, IL, IQEID, IQLID, IQTRC, IU, KODE, LFB, LU, MS, MUX,
     $        MYX, N, NO, NS
      logical LININT
      character BLANK*1, F*1, FACELAB*10, LINE*13, STAR*1
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(  2),MS   )
      equivalence (KZQ(  3),NS   )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
      equivalence (SYMBS(45),STAR  )
C
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
      equivalence (IQQ( 57),IQLID)
      equivalence (IQQ( 58),IQEID)
      equivalence (IQQ(299),IQTRC)
C     !EJECT
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C
C---- LINUS       as of 2004 May 12
      integer     LINKDS
      dimension   LINKDS(22)
      common      /LINUS/ LINKDS
C     Line source function calculation control parameters for the
C     current transition as set up by "PET" (and printed by "LINSEED").
C     IU    - index of upper level
C     IL    - index of lower level
C     KLIN  - line "type" code (1: radiative, 2: passive, etc)
C     ICE   - PRD calculation control
C     IPRO  - emergent profiles calculation control
C     METSE - statistical equilibrium calculation method selector
C     METSF - LSF calculation method selector (QR, RT, GR)
C     IBRSW - damping components selector
C     INKSW - input opacity signal
C     LSFT  - LSF solution code (0: full, 1:direct, etc)
C     ILFLX - line flux calculation control
C     LDL   - number of line components
C     LINT  - frequency integration range (half vs. full profile)
C     LSFP  - LSF printout control
C     IFDB  - LSF background control (constant vs. varying)
C     ISBG  - blended line profile plot mode switch
C     KBT   - length of input table XIBLUT
C     KRT   - length of input table XIREDT
C     KST   - length of input table XISYMT
C     KTRN  - length of actual tables XI and DL
C     LOML  - "line-background-continuum-opacity" control
C     ....  - (available)
      equivalence (LINKDS( 1),IU   )
      equivalence (LINKDS( 2),IL   )
C
C---- MOSTAR      as of 2000 Sep 26
      real*8      COREWL,COREWN
      integer     ICORE
      character   WLAB1*10,WLAB2*2,WLAB3*12,WLAB4*10,WLAB5*2
      logical     WAVENO,SLINE,WHOLE,RED,BLUE
      common      /MOSTAR1/ COREWL,COREWN
      common      /MOSTAR2/ ICORE
      common      /MOSTAR3/ WLAB1,WLAB2,WLAB3,WLAB4,WLAB5
      common      /MOSTAR4/ WAVENO,SLINE,WHOLE,RED,BLUE
C     Wavelength/Wavenumber print/plot controls (subroutine WUMBLE).
C     .
C     !DASH
      external ZEUS, ABJECT, TISRIT, LINER, TUMBLE, MARKI, HI, BYE
C
C               TAU(N), TMU(N), SNU(N), WS(N)
      dimension TAU(*), TMU(*), SNU(*), WS(*)
C     !EJECT
C
      call HI ('PINK')
C     !BEG
      LU = 0
      if(SLINE) then
        if((100*IU+IL).eq.(100*MS+NS)) then
          call ZEUS  (NO, IQLID, LU)
        end if
      else
        if(LININT) then
          call ZEUS  (NO, IQLID, LU)
        else
          call ZEUS  (NO, IQEID, LU)
        end if
      end if
C
      if(LU.gt.0) then
        call ABJECT  (LU)
        call TUMBLE  (LFB, FACELAB)
        write (LU,100) WLAB1(2:),WTAB,EM,FACELAB
  100   format(' ','Intensity Dump'//
     $         ' ','W',A,1X,F14.6,3X,'Mu',F11.6,5X,A10)
        if(.not.SLINE) then
          if(LININT.and.(IQTRC.gt.0)) then
            write (LU,101)
  101       format(' ','"True" continuum')
          end if
        end if
C
        call TISRIT  (MUX, YY, MYX, LINE)
        call LINER   (1, LU)
        write (LU,102) YNT,KODE,LINE
  102   format(' ','Intensity',1PE17.8,2X,'(',I1,')  at ',A13//
     $         ' ',22X,'TAU',17X,'TMU',17X,'SNU',18X,'WS',14X,'SNU*WS')
        call LINER   (1, LU)
C
        do 104 I = 1,N
          call MARKI (I, MUX, F, STAR, BLANK)
          WSNU = WS(I)*SNU(I)
          write (LU,103) I,TAU(I),TMU(I),SNU(I),WS(I),WSNU,F
  103     format(' ',I5,1P5E20.10,A1)
  104   continue
C
      end if
C     !END
      call BYE ('PINK')
C
      return
      end
