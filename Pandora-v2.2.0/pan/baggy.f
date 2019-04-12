      subroutine BAGGY
     $(NO,N,NL,NT,GMASS,Z,TE,CRL,CRT,CRH,KCRH,HND,XNE,XNP,VT,PTO,TPF,
     $ GD,RHMFF,RHFF,KHFF,CONA,CONX,KCOND,MODE,SUMH,SUM,RLINS,KLNS,
     $ XRAY,KRAY,COL,KCOL,SUMSM)
C
C     Rudolf Loeser, 1979 Oct 24
C---- Prints cooling rates.
C     !DASH
      save
C     !DASH
      real*8 COL, CONA, CONX, CRH, CRL, CRT, GD, GMASS, HND, PTO, RHFF,
     $       RHMFF, RLINS, SUM, SUMH, SUMSM, TE, TPF, VEC, VT, XNE, XNP,
     $       XRAY, Z
      integer I, IE, IL, IQHSE, IS, IU, IUL, J, KCOL, KCOND, KCRH, KHFF,
     $        KLIN, KLNS, KRAY, MODE, N, NL, NO, NODE, NOION, NT
      logical HSE, HYDR
      character BLANK*1, MARK*1, QCOL*15, QCONA*15, QCONX*15, QD*15,
     $          QELSM*8, QGD*15, QH*15, QHFF*15, QHMFF*15, QLINS*15,
     $          QM*15, QNE*15, QNH*15, QNP*15, QPF*15, QPT*15, QSM*15,
     $          QSU*15, QT*15, QTH*15, QVT*15, QXRAY*15, QZ*15, STAR*1
C     !COM
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
      equivalence (LINKDS( 3),KLIN )
C     !EJECT
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (QZQ(  2),QELSM)
      equivalence (KZQ( 94),NOION)
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
      equivalence (IQQ( 16),IQHSE)
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
      equivalence (SYMBS(45),STAR  )
C     !DASH
C     !EJECT
      external  MADAI, LINER, INTRANS, MIMSY, PET, AZURITE, HI, BYE
      intrinsic min
C
C               GMASS(N), Z(N), TE(N), CRL(N,NL), GD(N), CRH(N), VT(N),
      dimension GMASS(*), Z(*), TE(*), CRL(N,*),  GD(*), CRH(*), VT(*),
C
C               HND(N), XNE(N), XNP(N), PTO(N), TPF(N), COL(N), SUM(N),
     $          HND(*), XNE(*), XNP(*), PTO(*), TPF(*), COL(*), SUM(*),
C
C               RHMFF(N), RHFF(N), CONA(N), CONX(N), SUMH(N), RLINS(N),
     $          RHMFF(*), RHFF(*), CONA(*), CONX(*), SUMH(*), RLINS(*),
C
C               SUMSM(N), XRAY(N), CRT(N,NT)
     $          SUMSM(*), XRAY(*), CRT(N,*)
C
      dimension MARK(8), VEC(8)
C
      data QD,QM,QT        /'Depth', 'Mass', 'Temperature'/
      data QH,QNH,QNE      /'H-', 'Total Hydrogen', 'Electrons'/
      data QNP,QVT         /'Protons', 'Turb. Velocity'/
      data QPT,QPF         /'Total Pressure', 'Turb. Fraction'/
      data QGD,QSU         /'Gas Density', 'Total Rate'/
      data QHFF,QHMFF      /'H (free-free)', 'H- (free-free)'/
      data QCONA,QCONX     /'Cond. (approx.)', 'Cond. (general)'/
      data QTH,QLINS,QXRAY /'Total H', 'Composite Lines', 'X-ray'/
      data QCOL,QSM        /'CO-lines', 'Smoothed Rate'/
C
      call HI ('BAGGY')
C     !BEG
      if(NO.gt.0) then
        HSE  = IQHSE.gt.0
        HYDR = QELSM.eq.'H  '
        call MADAI   (NO,MODE,'RADIATIVE','COOLING')
        call AZURITE (NO,HYDR)
C     !EJECT
        IE = 0
  100   continue
          IS = IE+1
          IE = min(IE+8,N)
          call LINER         (2,NO)
          write (NO,101) (I,I=IS,IE)
  101     format(' ',15X,8I13)
  102     format(' ',A15,1P8E13.5)
          call LINER         (1,NO)
          write (NO,102) QD,(Z(I),I=IS,IE)
          write (NO,102) QT,(TE(I),I=IS,IE)
          if(HSE) then
            write (NO,102) QGD,(GD(I)   ,I=IS,IE)
            write (NO,102) QVT,(VT(I)   ,I=IS,IE)
            write (NO,102) QM ,(GMASS(I),I=IS,IE)
            write (NO,102) QNH,(HND(I)  ,I=IS,IE)
            write (NO,102) QNE,(XNE(I)  ,I=IS,IE)
            write (NO,102) QNP,(XNP(I)  ,I=IS,IE)
            write (NO,102) QPT,(PTO(I)  ,I=IS,IE)
            write (NO,102) QPF,(TPF(I)  ,I=IS,IE)
          end if
          call MIMSY         (NO,MODE)
          if(NOION.le.0) then
            do 104 J = 1,NL
              write (QZ,103) J
  103         format(I2,'/ K',10X)
              write (NO,102) QZ,(CRL(I,J),I=IS,IE)
  104       continue
            call LINER       (1,NO)
            do 106 J = 1,NT
              call PET       (J)
              if(KLIN.eq.1) then
                call INTRANS (IU,IL,'BAGGY',IUL)
                write (QZ,105) IU,IL
  105           format(I2,'/',I2,10X)
                write (NO,102) QZ,(CRT(I,IUL),I=IS,IE)
              end if
  106       continue
          end if
C     !EJECT
          if(KHFF.eq.1) then
            call LINER   (1,NO)
            write (NO,102) QHFF,(RHFF(I),I=IS,IE)
          end if
          if(HYDR) then
            call LINER   (1,NO)
            write (NO,102) QTH,(SUMH(I),I=IS,IE)
          end if
          NODE = 0
          if(KCRH.eq.1) then
            call LINER   (1,NO)
            write (NO,102) QH,(CRH(I),I=IS,IE)
            NODE = 1
          end if
          if(KHFF.eq.1) then
            if(NODE.eq.0) then
              call LINER (1,NO)
            end if
            write (NO,102) QHMFF,(RHMFF(I),I=IS,IE)
          end if
          if(KCOND.eq.1) then
            call LINER   (1,NO)
            write (NO,102) QCONA,(CONA(I),I=IS,IE)
            write (NO,102) QCONX,(CONX(I),I=IS,IE)
          end if
          if(KLNS.gt.0) then
            call LINER   (1,NO)
            write (NO,102) QLINS,(RLINS(I),I=IS,IE)
          end if
          if(KRAY.gt.0) then
            call LINER   (1,NO)
            write (NO,102) QXRAY,(XRAY(I),I=IS,IE)
          end if
          if(KCOL.gt.0) then
            call LINER   (1,NO)
            write (NO,102) QCOL,(COL(I),I=IS,IE)
          end if
          call LINER     (1,NO)
          write (NO,102) QSU,(SUM(I),I=IS,IE)
C     !EJECT
          if(MODE.eq.1) then
            J = 0
            do 107 I = IS,IE
              J = J+1
              VEC(J) = SUMSM(I)
              if(VEC(J).eq.SUM(I)) then
                MARK(J) = BLANK
              else
                MARK(J) = STAR
              end if
  107       continue
            write (NO,108) QSM,(VEC(I),MARK(I),I=1,J)
  108       format(' ',A15,1PE13.5,A1,7(E12.5,A1))
          end if
        if(IE.lt.N) goto 100
      end if
C     !END
      call BYE ('BAGGY')
C
      return
      end
