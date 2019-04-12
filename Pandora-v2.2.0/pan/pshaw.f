      subroutine PSHAW
     $(NO,W,N,EP,DEL,B,BF,BS,BA,S,ST,TAU,RHO,ANT,T,UNT,XJBAR,FNDT,
     $ Z,DAMPY,YCONT,FRS,MSFT,ISB1,ISB2,GMAI,GMMA,DRLIMI,XR,TAUM,
     $ KSE,KSEDA,SD,ED,BD,RULED,SN,AW,IDDL,XKL,XKT,COP,CSF,SL,BC,
     $ XND,CNDT,LINE)
C
C     Rudolf Loeser, 1980 May 01
C           revised, 2004 May 10
C
C---- Prints results of Line Source Function calculation.
C     (This is version 2 of PSHAW.)
C     !DASH
      save
C     !DASH
      real*8 ANT, AW, B, BA, BC, BD, BF, BS, CNDT, COP, CSF, DAMPY, DEL,
     $       DELLM, DRLIMI, ED, EP, FNDT, FRS, GMAI, GMMA, RHO, RULED,
     $       S, SD, SL, SN, ST, T, TAU, TAUM, UNT, W, XJBAR, XKL, XKT,
     $       XND, XR, YCONT, Z
      integer IBL, ICE, IDDL, IFNP, IJBRL, IN, IP, IQASP, IQLSG, IQLSP,
     $        IRULP, IS, ISB1, ISB2, ISL, ISTL, IVEC, IZL, KSE, KSEDA,
     $        LSFGC, LSFP, LU, MOX, MSFT, N, NO
      logical GRAPH, INCI, SHORT, TABLE
      character LINE*(*)
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
      equivalence (LINKDS( 4),ICE  )
      equivalence (LINKDS(14),LSFP )
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
      equivalence (KZQ(108),LSFGC)
      equivalence (RZQ(177),DELLM)
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
      equivalence (IQQ(250),IQLSG)
      equivalence (IQQ(287),IQLSP)
      equivalence (IQQ(259),IQASP)
C     !DASH
C     !EJECT
      external ABJECT, VOGUL, KALAN, ALOIS, CUMIN, SILOA, BLOOP, DIGON,
     $         ORINOCO, VISTA, BOTTLE, SWIFT, WGIVE, HI, BYE
C
      dimension W(*)
C
C               EP(N), DEL(N), ANT(N), UNT(N), BS(N), GMAI(N), TAUM(N),
      dimension EP(*), DEL(*), ANT(*), UNT(*), BS(*), GMAI(*), TAUM(*),
C
C               TAU(N), RHO(N), XJBAR(N), FNDT(N), Z(N), ST(N), FRS(N),
     $          TAU(*), RHO(*), XJBAR(*), FNDT(*), Z(*), ST(*), FRS(*),
C
C               SN(N), DRLIMI(N), SD(N), ED(N), BD(N), BA(N), RULED(N),
     $          SN(*), DRLIMI(*), SD(*), ED(*), BD(*), BA(*), RULED(*),
C
C               B(N), S(N), AW(N), BF(N), T(N), XKL(N), COP(N), XKT(N),
     $          B(*), S(*), AW(*), BF(*), T(*), XKL(*), COP(*), XKT(*),
C
C               CSF(N), SL(N), BC(N), CNDT(N), XND(N,NL)
     $          CSF(*), SL(*), BC(*), CNDT(*), XND(N,*)
C
      dimension IN(9)
      equivalence
     $(IN( 1),IFNP  ),(IN( 2),IP    ),(IN( 3),IZL   ),(IN( 4),ISL   ),
     $(IN( 5),IBL   ),(IN( 6),IJBRL ),(IN( 7),ISTL  ),(IN( 8),IRULP ),
     $(IN( 9),IVEC  )
C
      call HI ('PSHAW')
C     !BEG
      if(NO.gt.0) then
C       (Get, and allocate, W allotment)
        call CUMIN (IN, IS, MOX, 'PSHAW')
C
        TABLE = (LSFP.gt.0).or.(IQLSP.gt.0)
        GRAPH = (LSFP.gt.0).or.(IQLSG.gt.0)
        SHORT = IQASP.gt.0
C     !EJECT
        if(TABLE) then
C----     Prepare FNDT for printing
          call BLOOP    (N, FNDT, EP, DEL, W(IFNP), INCI)
C----     Print main table
          call ORINOCO  (NO, N, EP, DEL, B, BS, BA, BF, S, TAU, RHO,
     $                   XJBAR, W(IFNP), INCI, SHORT, IDDL, DELLM)
C----     Print Source Function control parameters
          call VOGUL    (NO, DAMPY, YCONT, MSFT, ISB1, ISB2)
          if((.not.SHORT).and.(MSFT.ne.2)) then
            call BOTTLE (NO, LU)
C----       Print details of total source function, ST
            call VISTA  (LU, N, XKL, SL, COP, CSF, XKT, ST, LINE)
C----       Print auxiliary data
            call KALAN  (LU, N, FRS, S, XJBAR, AW, EP, BS, ICE, GMMA,
     $                   XR, ANT, T, UNT, GMAI, DRLIMI, SN, W(IP),
     $                   W(IVEC))
            call SWIFT  (LU, CSF, BC, B, SN, CNDT, XND)
          end if
C----     Print source function comparisons
          call DIGON    (NO, N, MSFT, S, SD, KSE, KSEDA, W(IVEC))
          if(GRAPH) then
            call ABJECT (NO)
          end if
        end if
C
        if(GRAPH) then
          if((LSFGC.eq.1).or.(LSFGC.eq.3)) then
C----       Graph vs. Z-index
            call ALOIS  (1, N, Z, S, B, XJBAR, ST, TAU, TAUM, W(IZL),
     $                   W(ISL), W(IBL), W(IJBRL), W(ISTL), RULED,
     $                   W(IRULP), NO)
          end if
          if((LSFGC.eq.2).or.(LSFGC.eq.3)) then
C----       Graph vs. Z
            call ALOIS  (2, N, Z, S, B, XJBAR, ST, TAU, TAUM, W(IZL),
     $                   W(ISL), W(IBL), W(IJBRL), W(ISTL), RULED,
     $                   W(IRULP), NO)
          end if
C----     Diffusion Analysis
          call SILOA    (N, KSE, KSEDA, Z, S, SD, EP, ED, BS, BD,
     $                   W(IZL), W(ISL), W(IBL), W(IFNP), W(IP),
     $                   W(IJBRL), W(ISTL), NO)
        end if
C
C       (Give back W allotment)
        call WGIVE      (W, 'PSHAW')
      end if
C     !END
      call BYE ('PSHAW')
C
      return
      end
