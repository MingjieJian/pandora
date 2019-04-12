      subroutine SWAMP
     $(LU,MODE,N,NL,NT,Z,GMASS,TE,CRT,CRL,CRH,KCRH,HND,XNE,XNP,VT,PTO,
     $ TPF,GD,RHMFF,RHFF,KHFF,COND,KCOND,SUM,RLINS,KLNS,XRAY,KRAY,COL,
     $ KCOL,SUMSM)
C
C     Rudolf Loeser, 1979 Oct 25
C---- Saves cooling rates data.
C     !DASH
      save
C     !DASH
      real*8 COL, COND, CRH, CRL, CRT, GD, GMASS, HND, PTO, RHFF, RHMFF,
     $       RLINS, SUM, SUMSM, TE, TPF, VT, XNE, XNP, XRAY, Z
      integer IL, IQHSE, IU, IUL, J, KCOL, KCOND, KCRH, KHFF, KLIN,
     $        KLNS, KOOLS, KRAY, LU, MODD, MODE, N, NL, NOION, NT
      character QCOL*8, QCOND*8, QCRH*8, QCRL*8, QCRT*8, QGD*8, QHFF*8,
     $          QHMFF*8, QLINS*8, QM*8, QNE*8, QNH*8, QNP*8, QPTO*8,
     $          QSUM*8, QSUMSM*8, QT*8, QTPF*8, QVT*8, QXRAY*8, QZ*8
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
C
C---- HEADER      as of 1984 Apr 23
      character   HEAD*80
      common      /HEADER/ HEAD
C     Copy of the "HEADER" line for this run.
C     .
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
      equivalence (KZQ( 94),NOION)
      equivalence (KZQ( 86),KOOLS)
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
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
C     !DASH
C     !EJECT
      external SPAWN, BUNT, PENT, PANT, PET, INTRANS, HALT, HI, BYE
C
C               CRT(N,NT), XRAY(N), CRL(N,NL), COND(N), GMASS(N), Z(N),
      dimension CRT(N,*),  XRAY(*), CRL(N,*),  COND(*), GMASS(*), Z(*),
C
C               HND(N), XNE(N), XNP(N), VT(N), PTO(N), TPF(N), RHFF(N),
     $          HND(*), XNE(*), XNP(*), VT(*), PTO(*), TPF(*), RHFF(*),
C
C               RLINS(N), RHMFF(N), SUM(N), CRH(N), COL(N), SUMSM(N),
     $          RLINS(*), RHMFF(*), SUM(*), CRH(*), COL(*), SUMSM(*),
C
C               GD(N), TE(N)
     $          GD(*), TE(*)
C
      data QZ, QM, QT, QCRT        /'Z', 'MASS', 'TE', 'CRT'/
      data QCRH, QNH, QNE, QNP     /'CRH', 'NH', 'NE', 'NP'/
      data QVT, QPTO, QTPF, QGD    /'VT', 'PTO', 'TPF', 'GD'/
      data QHFF, QHMFF, QSUM ,QCOL /'HFF', 'H-FF', 'SUM', 'CO'/
      data QCOND, QLINS, QSUMSM    /'COND', 'LINES', 'SUMSM'/
      data QXRAY, QCRL             /'XRAY', 'CRL'/
C
      data MODD /1/
C
      call HI ('SWAMP')
C     !BEG
      if(MODE.eq.1) then
C----   Save data for possible temperature correction calculation
        call SPAWN (N,KOOLS,XNE,HND,TE,SUM,SUMSM)
      end if
C     !EJECT
      if(LU.gt.0) then
C----   Write general cooling rates data save file
        if(MODE.eq.1) then
          write (LU,100) HEAD
  100     format(A80)
          write (LU,101) N,NT,NL,KOOLS
  101     format(4I5,5X,'= N, NT, NL, KOOLSUM')
C
          call BUNT        (LU,Z ,QZ)
          call BUNT        (LU,TE,QT)
          if(IQHSE.gt.0) then
            call BUNT      (LU,GD   ,QGD)
            call BUNT      (LU,GMASS,QM)
            call BUNT      (LU,HND  ,QNH)
            call BUNT      (LU,XNE  ,QNE)
            call BUNT      (LU,XNP  ,QNP)
            call BUNT      (LU,VT   ,QVT)
            call BUNT      (LU,PTO  ,QPTO)
            call BUNT      (LU,TPF  ,QTPF)
          end if
C
          write (LU,102)
  102     format('[   C O O L I N G   ] > ')
        else if(MODE.eq.2) then
          write (LU,103)
  103     format('[   Integrated  C O O L I N G   ] > ')
        else
          write (MSSLIN(1),104) MODE
  104     format('MODE =',I12,', which is neither 1 nor 2.')
          call HALT        ('SWAMP',1)
        end if
C
        if(NOION.le.0) then
          write (LU,105)
  105     format('[   CRT = radiative transitions   ] > ')
          do 106 J = 1,N
            call PET       (J)
            if(KLIN.eq.1) then
              call INTRANS (IU,IL,'SWAMP',IUL)
              call PENT    (LU,CRT(1,IUL),IU,IL,QCRT)
            end if
  106     continue
          write (LU,107)
  107     format('[   CRL = transitions to continuum   ] > ')
          call PANT        (LU,CRL,N,NL,MODD,QCRL)
        end if
C     !EJECT
        if(KHFF.eq.1) then
          write (LU,108)
  108     format('[   HFF = H free-free   ] > ')
          call BUNT   (LU,RHFF ,QHFF)
        end if
        if(KCOND.eq.1) then
          write (LU,109)
  109     format('[   COND = conduction   ] > ')
          call BUNT   (LU,COND ,QCOND)
        end if
        if(KCRH.gt.0) then
          write (LU,110)
  110     format('[   CRH = H-   ] > ')
          call BUNT   (LU,CRH  ,QCRH)
          if(KHFF.eq.1) then
            write (LU,111)
  111       format('[   H-FF = H- free-free   ] > ')
            call BUNT (LU,RHMFF,QHMFF)
          end if
        end if
        if(KLNS.gt.0) then
          write (LU,112)
  112     format('[   LINES = Composite Lines   ] > ')
          call BUNT   (LU,RLINS,QLINS)
        end if
        if(KRAY.gt.0) then
          write (LU,113)
  113     format('[   XRAY = X-rays   ] > ')
          call BUNT   (LU,XRAY ,QXRAY)
        end if
        if(KCOL.gt.0) then
          write (LU,114)
  114     format('[   CO = CO-lines   ] > ')
          call BUNT   (LU,COL  ,QCOL)
        end if
        write (LU,115)
  115   format('[   SUM = total rate   ] > ')
        call BUNT     (LU,SUM  ,QSUM)
        if(MODE.eq.1) then
          write (LU,116)
  116     format('[   SUMSM = smoothed rate   ] > ')
          call BUNT   (LU,SUMSM,QSUMSM)
        end if
      end if
C     !END
      call BYE ('SWAMP')
C
      return
      end
