      subroutine ELIS
     $(K,XI,XJNU,RXI,XRD,ZRD,YRD,XKPC,BC,SIG,FRD,GRD,STZ)
C
C     Rudolf Loeser, 1986 Dec 17
C---- Saves PRD data for use by Line Source Function calculation.
C     (This is version 3 of ELIS.)
C     !DASH
      save
C     !DASH
      real*8 BC, FRD, GRD, RXI, SIG, STZ, XI, XJNU, XKPC, XRD, YRD, ZRD
      integer ICE, IL, IQPD3, IU, J, K, LEN, N
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
      equivalence (LINKDS( 4),ICE  )
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
      equivalence (IQQ(206),IQPD3)
C     !DASH
      external FOG, TING, ITCH, HI, BYE
C
C               BC(N,K), YRD(N,K), XI(K), FRD(N,K), GRD(N,K), STZ(N,2),
      dimension BC(N,*), YRD(N,*), XI(*), FRD(N,*), GRD(N,*), STZ(N,*),
C
C               XJNU(N,K), RXI(N,K), XKPC(N,K), XRD(N,K), SIG(N,K),
     $          XJNU(N,*), RXI(N,*), XKPC(N,*), XRD(N,*), SIG(N,*),
C
C               ZRD(N,K)
     $          ZRD(N,*)
C
      call HI ('ELIS')
C     !BEG
C---- Write arrays
      LEN = N*K
      call FOG      (1, IU, IL, LEN, XKPC, BC, SIG, XJNU, RXI, XRD,
     $               ZRD, YRD)
C
C---- Save line-center data for total source function (ST) calculation
      call TING     (N, K, ICE, XI, XJNU, RXI, FRD, GRD, STZ)
C
      if(IQPD3.gt.0) then
C----   (Dump)
        do 100 J = 1,K
          call ITCH ('ELIS', IU, IL, J, K, N, XKPC(1,J), BC(1,J),
     $               SIG(1,J), XJNU(1,J), RXI(1,J), XRD(1,J),
     $               ZRD(1,J), YRD(1,J))
  100   continue
      end if
C     !END
      call BYE ('ELIS')
C
      return
      end
