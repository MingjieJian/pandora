      subroutine BROOM
     $(Z,TE,N,WTAB,ZTM,K,LU)
C
C     Rudolf Loeser, 1992 Dec 30
C---- Graph of depths-of-formation for a line profile.
C     (ZTM is set up by subroutine BAUCIS.)
C     (This is version 2 of BROOM.)
C     !DASH
      save
C     !DASH
      real*8 TE, WL, WR, WTAB, Z, ZERO, ZH, ZL, ZTM
      integer IL, IU, K, LOGAS, LU, N, NH, NV
      logical GOOD, INDX, VALU
      character NUMERO*1, PERIOD*1, STAR*1
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
      equivalence (KZQ(138),LOGAS)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(38),NUMERO)
      equivalence (SYMBS(42),PERIOD)
      equivalence (SYMBS(45),STAR  )
C
C---- IMAGE       as of 1997 Aug 21
      integer     IMALEN
      parameter   (IMALEN=65535)
      character   IMAGE*(IMALEN)
      common      /IMAGE/ IMAGE
C     Character string to hold plot images constructed by the
C     K-type line printer plotting routines;
C     but used also as a general scratch character array.
C     .
C     !EJECT
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
      external  ALDEN, KINIT, KRIGIA, KLINEC, BAKAIRI, ABJECT, LINER,
     $          VALE, HI, BYE
C
C               Z(N), TE(N), WTAB(KM), ZTM(KM,4)
      dimension Z(*), TE(*), WTAB(*),  ZTM(*)
C
      data NV,NH /51,101/
C     !EJECT
C
      call HI ('BROOM')
C     !BEG
      if((LU.gt.0).and.((LOGAS.ge.1).and.(LOGAS.le.2))) then
        INDX = LOGAS.eq.1
        VALU = LOGAS.eq.2
C----   Set up graph limits
        if(INDX) then
          WL = 1
          WR = K
        else if(VALU) then
          WL = WTAB(1)
          WR = WTAB(K)
        end if
        call VALE     (ZTM,K,ZL,ZH)
C----   Initialize graph image
        call KINIT    (IMAGE,WL,WR,ZL,ZH,NV,NH,NUMERO,GOOD)
        if(.not.GOOD) then
          call KRIGIA (WL,WR,ZL,ZH,NV,NH)
        end if
        if((WL.lt.ZERO).and.(WR.gt.ZERO)) then
          call KLINEC (IMAGE,ZERO,ZL,ZERO,ZH,PERIOD,0)
        end if
C
C----   Enter data into graph image
        call BAKAIRI  (IMAGE,WTAB,ZTM,K,INDX,VALU)
C
C----   Print
        call ABJECT   (LU)
        if(INDX) then
          write (LU,100) WLAB3,' index ',IU,IL,LOGAS
        else if(VALU) then
          write (LU,100) WLAB3,' ',IU,IL,LOGAS
        end if
  100   format(' ','Analysis of location versus ',A12,A,'for the (',
     $             I2,'/',I2,') line.',5X,'LOGAS =',I2//
     $         ' ',7X,'Z',10X,'TE(Z)')
        call ALDEN    (LU,IMAGE,NV,ZL,ZH,Z,TE,N)
        if(INDX) then
          write (LU,101) 1,K
  101     format(' ',25X,I4,94X,I4)
        else if(VALU) then
          write (LU,102) WL,WR
  102     format(' ',25X,1PE11.4,81X,E10.4)
        end if
        call LINER    (1,LU)
        write (LU,103) STAR,ALPHS(1),ALPHS(2),ALPHS(3)
  103   format(' ',26X,A1,' is the locus of Z(KS);'/
     $         ' ',26X,A1,' is the locus of TMU(Z) = 0.10;'/
     $         ' ',26X,A1,' is the locus of TMU(Z) = 1.00;'/
     $         ' ',26X,A1,' is the locus of TMU(Z) = 10.0.')
      end if
C     !END
      call BYE ('BROOM')
C
      return
      end
