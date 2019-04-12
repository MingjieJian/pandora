      subroutine DUBRAS
     $(NDR,XDR,DDR,KDDR,KDRX,KNZGM,NO)
C
C     Rudolf Loeser, 1980 Mar 13
C---- Prints PRD parameters.
C     !DASH
      save
C     !DASH
      real*8 DDR, DRLIM, PRDCV, TAUCL, XCL, XDR
      integer I, IDRDP, IGII, IGMSW, IPRDD, IPRDF, IQPMH, ITPRD, KDDR,
     $        KDRDP, KDRX, KMH, KNEW, KNZGM, KOLD, NDR, NO, NONC
      character BLANK*1, PLAB*16, QELSM*8, qummy*8
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
      equivalence (RZQ(102),XCL  )
      equivalence (RZQ(103),TAUCL)
      equivalence (KZQ( 98),IPRDD)
      equivalence (KZQ( 99),IPRDF)
      equivalence (KZQ(102),IDRDP)
      equivalence (KZQ(103),KDRDP)
      equivalence (RZQ( 22),DRLIM)
      equivalence (KZQ(183),IGMSW)
      equivalence (QZQ(  2),QELSM)
      equivalence (KZQ(213),IGII )
      equivalence (RZQ( 66),PRDCV)
      equivalence (KZQ(214),ITPRD)
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(29),NONC )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !EJECT
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
      equivalence (IQQ(337),IQPMH)
C     !DASH
      external  PADMA, VECOUT, DASHER, LINER, ONOFF, DONNA, HI, BYE
      intrinsic min, max
C
C               XDR(NDR), DDR(NDR)
      dimension XDR(*),   DDR(*)
C
      dimension PLAB(2)
C
      data PLAB /'Kneer & Heasley.', 'Hubeny & Lites.'/
C
      call HI ('DUBRAS')
C     !BEG
      if((NO.gt.0).and.(NONC.gt.0)) then
        call PADMA   (NO, 'P.R.D. Calculation')
        call ONOFF   (IQPMH, KMH, qummy)
        call DONNA   (KOLD, KNEW)
C
        write (NO,100) PLAB(KMH+1)
  100   format(' ','Option PRDMETH: this run uses the formulation ',
     $             'of ',A//
     $         ' ','(For more information about PRD calculations, ',
     $             'see Section 15 of "About PANDORA.")')
        if((KOLD*KNEW).gt.0) then
          call LINER (1, NO)
          write (NO,101) PLAB(1)
  101     format(' ','NOTE: ',A,' is used for blended lines.')
        end if
C     !EJECT
        if(KOLD.gt.0) then
          call LINER   (1, NO)
          call DASHER  (NO)
          call LINER   (1, NO)
          write (NO,102) DRLIM
  102     format(' ','Calculation of DR(x), the absorption fraction ',
     $               'as a function of distance from line center ',
     $               '(in Doppler widths).'//
     $           ' ','DR(x), for each transition, can be obtained ',
     $               'by one of three methods:'/
     $           ' ','(1) by a formula, (2) by the formula with ',
     $               'alternate parameters, or (3) from a table.'//
     $           ' ','***  NOTE: values of XC(u,l), and other ',
     $               'transition-specific PRD parameters, appear in ',
     $               'the ATOM printout.'/
     $           ' ',21X,'DRLIM =',1PE12.4)
          if(KDDR.gt.0) then
            call LINER (1, NO)
            write (NO,103)
  103       format(' ','DR table (used when XC(u,l) = -1)'//
     $             ' ',11X,'XDR (=x)',6X,'DDR (=DR)')
            call LINER (1, NO)
            write (NO,104) (I,XDR(I),DDR(I),I=1,NDR)
  104       format(5(' ',4X,I3,F15.4,F15.7/))
          end if
          if(KDRX.gt.0) then
            call LINER (1, NO)
            write (NO,105) XCL,TAUCL
  105       format(' ','Alternate DR calculation parameters (used ',
     $                 'when XC(u,l) = 0)'//
     $             ' ','XCL =',1PE12.4,5X,'TAUCL =',E12.4)
          end if
        end if
C
        if(KNEW.gt.0) then
          call LINER  (1, NO)
          call DASHER (NO)
          call LINER  (1, NO)
          write (NO,106) IGII
  106     format(' ','The switch IGII controls how the function GII ',
     $               'is calculated;'/
     $           ' ','IGII = 1 means: use the fast approximation of ',
     $               'Gouttebroze;'/
     $           ' ','IGII = 2 means: use the more accurate, slower, ',
     $               'subroutine of Adams, Hummer & Rybicki.'/
     $           ' ','In this run IGII =',I2)
          call LINER  (2, NO)
          write (NO,107) ITPRD,PRDCV
  107     format(' ','PRD-iterations: limit ITPRD =',I3,
     $               ', convergence criterion PRDCV =',1PE8.1)
        end if
C     !EJECT
        call LINER    (1, NO)
        call DASHER   (NO)
        call LINER    (1, NO)
        if(IGMSW.le.0) then
          write (NO,108) IGMSW
        else
          if(QELSM(1:3).eq.'H  ') then
            write (NO,108) IGMSW,BLANK
          end if
        end if
  108   format(' ','IGMSW =',I2,:,A,'the alternate formulation of ',
     $             'GMMA(i) for Lyman alpha and beta will be used.')
C
        call LINER    (3, NO)
        write (NO,109) IPRDD,IPRDF,IDRDP,KDRDP
  109   format(' ','Also note options: ','JNUPRNT'/
     $         ' ',19X,'PRDCOPR'/
     $         ' ',19X,'PRDPRNT, which uses IPRDD (=',I5,') and IPRDF ',
     $             '(=',I5,')'/
     $         ' ',19X,'  DRDMP, which uses IDRDP (=',I5,') and KDRDP ',
     $             '(=',I5,') for transition (MS/NS)')
C
      end if
C     !END
      call BYE ('DUBRAS')
C
      return
      end
