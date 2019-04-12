      subroutine DIANA
     $(X,IX,W,IW,DBLOC,ILFLX,MPROM,DAMPY,TAU,DL,K,DP,DW,DDL,CDL,GTN,
     $ COP,BC,Z,XI,A,VXS,IMG,SCNU,XKCNU,XNE,FDDL,MEDUSA,DMP0,DMP1,
     $ DMP2,KSE,KSEDA,C,EXT,ANT,AW,PA,PG,PB,OMD,DLC,PQ,PD,CSF,B,BTR)
C
C     Rudolf Loeser, 2004 May 06
C---- Computes frequency/angle sums for stationary atmospheres.
C     Upon return, KODE=1 if calculation was successful, =0 if not.
C
C     (This is version 4 of DIANA.)
C     !DASH
      save
C     !DASH
      real*8 A, ANT, AW, B, BC, BTR, C, CDL, COP, CSF, DAMPY, DBLOC,
     $       DDL, DL, DLC, DP, DW, EXT, FDDL, GTN, OMD, PA, PB, PD, PG,
     $       PQ, SCNU, TAU, VXS, W, X, XI, XKCNU, XNE, Z
      integer IFDB, IL, ILFLX, IMG, IQINC, IU, IW, IX, K, KODE, KSE,
     $        KSEDA, MEDUSA, MPROM, N
      logical DMP0, DMP1, DMP2, DUMP, GAW, INC
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
      equivalence (IQQ( 51),IQINC)
C     !EJECT
C---- URANUS      as of 2005 Dec 12
      integer     LEMUR
      parameter   (LEMUR=30000)
      integer     LIMPID,IUOP,ILOP,NBOP,MBOP,KEROP,INDOP
      real*8      OPNAM
      dimension   OPNAM(LEMUR),KEROP(LEMUR),INDOP(LEMUR)
      common      /URANUS1/ LIMPID,IUOP,ILOP,NBOP,MBOP
      common      /URANUS2/ KEROP
      common      /URANUS3/ INDOP
      common      /URANUS4/ OPNAM
C     Record index for Diana/Orion Data Blocks.
C
C     KEROP .eq. 0: acceptable data block, use it;
C     KEROP .gt. 0: defective data block, do not use it.
C     (The various values of KEROP signify different error conditions).
C     .
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
      equivalence (LINKDS(15),IFDB )
C     !DASH
C     !EJECT
      external QUEST, MAPLE, MESHED, MASHED, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               TAU(N), ANT(N), OMD(N), DL(KM), EXT(N), VXS(N), XNE(N),
      dimension TAU(*), ANT(*), OMD(*), DL(*),  EXT(*), VXS(*), XNE(*),
C
C               DW(N), DDL(LDL), CDL(LDL), GTN(N), COP(N), BC(N), C(N),
     $          DW(*), DDL(*),   CDL(*),   GTN(*), COP(*), BC(*), C(*),
C
C               PB(N), PA(N,N), PG(N,N), DBLOC(Lpdlen), DLC(N), IMG(N),
     $          PB(*), PA(*),   PG(*),   DBLOC(*),      DLC(*), IMG(*),
C
C               PQ(N), PD(N), SCNU(N,KM), A(KM), FDDL(N), Z(N), XI(KM),
     $          PQ(*), PD(*), SCNU(*),    A(*),  FDDL(*), Z(*), XI(*),
C
C               DP(N,LDL), XKCNU(N,KM), AW(N), CSF(N), BTR(N), B(N)
     $          DP(*),     XKCNU(*),    AW(*), CSF(*), BTR(*), B(*)
C     !EJECT
C
      call HI ('DIANA')
C     !BEG
      if(KSE.eq.1) then
C----   Set up auxiliary data in DBLOC records
        IUOP = IU
        ILOP = IL
        call QUEST    (X, W, IW, XI, A, Z, XNE, VXS, DAMPY, DP, DW,
     $                 DDL, FDDL, CDL, GTN, COP, TAU, BC, CSF, B, BTR,
     $                 DL, K, SCNU, XKCNU, IMG, DBLOC, N, DMP0, DMP1,
     $                 ILFLX, IFDB, MPROM, KODE)
      end if
      MEDUSA = KODE
C
      if(MEDUSA.gt.0) then
        DUMP = DMP1.or.DMP2
        GAW  = (KSEDA.eq.1).or.(KSE.eq.2)
        INC  = IQINC.gt.0
C
        if(DUMP) then
          call MESHED ('DIANA', 2)
        end if
C----   Do frequency/angle summations using the data in DBLOCs
        call MAPLE    (N, GTN, C, EXT, ANT, PA, PG, PB, OMD, DLC, PQ,
     $                 PD, INC, AW, GAW, DBLOC, DMP1, DMP2)
        if(DUMP) then
          call MASHED ('DIANA')
        end if
      end if
C     !END
      call BYE ('DIANA')
C
      return
      end
