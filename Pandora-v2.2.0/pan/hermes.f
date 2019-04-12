      subroutine HERMES
     $(X,IX,W,IW,TAU,RHO,RHOK,SN,S,ST,XJBAR,B,EP,DEL,BS,BA,BF,FNDT,Z,
     $ XND,VSB,FXI,YBRC,DP,DW,FRS,MSFT,ISB1,ISB2,COP,BTR,BC,CNDT,CSF,
     $ XKL,XKT,TAUM,VEC,RULED,IMG)
C
C     Rudolf Loeser, 1985 Apr 16
C---- Bypasses the full solution of the radiative transfer equation,
C     using the Escape Probability approximation.
C     !DASH
      save
C     !DASH
      real*8 B, BA, BC, BF, BS, BTR, CNDT, COP, CSF, DEL, DP, DW, EP,
     $       FNDT, FRS, FXI, RHO, RHOK, RULED, S, SN, ST, TAU, TAUM,
     $       VEC, VSB, W, X, XJBAR, XKL, XKT, XND, YBRC, Z, ZERO, dummy
      integer IDDL, IMG, ISB1, ISB2, IW, IX, KSE, KSEDA, LSFT, MO, MSFT,
     $        N
      character LINE*45
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
      equivalence (LINKDS(10),LSFT )
C     !EJECT
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external PIRET, ZERO1, SIOUX, TROCK, PSHAW, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               EP(N), DEL(N), B(N), BF(N), BS(N), BA(N), S(N), TAU(N),
      dimension EP(*), DEL(*), B(*), BF(*), BS(*), BA(*), S(*), TAU(*),
C
C               RHO(N), XJBAR(N), FNDT(N), Z(N), FRS(N), ST(N), VEC(N),
     $          RHO(*), XJBAR(*), FNDT(*), Z(*), FRS(*), ST(*), VEC(*),
C
C               YBRC(N), CSF(N), RULED(N), TAUM(N), RHOK(N), DW(N,LDL),
     $          YBRC(*), CSF(*), RULED(*), TAUM(*), RHOK(*), DW(*),
C
C               XKT(N), XKL(N), COP(N), FXI(N), IMG(N), VSB(N), BTR(N),
     $          XKT(*), XKL(*), COP(*), FXI(*), IMG(*), VSB(*), BTR(*),
C
C               SN(N), BC(N), DP(N), XND(N,NL), CNDT(*)
     $          SN(*), BC(*), DP(*), XND(*),    CNDT(*)
C
      data KSE,KSEDA  /0, 0/
C     !EJECT
C
      call HI ('HERMES')
C     !BEG
C---- Compute Escape Probability solution
      if(LSFT.ne.2) then
        ISB1 = 1
        ISB2 = 1
      end if
      call PIRET (N, TAU, SN, S, RHO, XJBAR, Z, VSB, FXI, YBRC, DP,
     $            DW, ISB1, ISB2, IMG, W)
C
C---- Take care of arrays that weren't computed
      call ZERO1 (EP,   N)
      call ZERO1 (DEL,  N)
      call ZERO1 (BS,   N)
      call ZERO1 (BA,   N)
      call ZERO1 (BF,   N)
      call ZERO1 (FNDT, N)
      call ZERO1 (RHOK, N)
      IDDL = N
C
C---- Set up "Continuum Source Function" (for profile calculation)
      call SIOUX (N, CSF, B, BTR, BC)
C---- Compute total source function (for plot)
      call TROCK (N, XKL, S, COP, CSF, XKT, ST, VEC)
      LINE = 'SL = S'
C
C---- Print
      call PSHAW (MO, W, N, EP, DEL, B, BF, BS, BA, S, ST, TAU, RHO,
     $            dummy, dummy, dummy, XJBAR, FNDT, Z, ZERO, dummy,
     $            FRS, MSFT, ISB1, ISB2, dummy, dummy, dummy, dummy,
     $            TAUM, KSE, KSEDA, dummy, dummy, dummy, RULED,
     $            dummy, dummy, IDDL, XKL, XKT, COP, CSF, S, BC,
     $            XND, CNDT, LINE)
C     !END
      call BYE ('HERMES')
C
      return
      end
