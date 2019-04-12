      subroutine BANANA
     $(NO,NR,Z,DW,DP,VX,DV,A,K,DL,PHI,U,DDL,FDDL,IHSSP,CDL,LDL,PHC,
     $ XNE,MPROM,VFI,VFC,ELL)
C
C     Rudolf Loeser, 1985 Jun 20
C---- Prints, for PEEK.
C     (This is version 3 of BANANA.)
C     !DASH
      save
C     !DASH
      real*8 A, CDL, DDL, DL, DP, DV, DW, ELL, FDDL, PHC, PHI, U, VFC,
     $       VFI, VX, XNE, Z, dummy
      integer I, IHSSP, IQBLN, K, LDL, LUEO, MPROM, NO, NR
      logical IVX
      character LAB*30
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
      equivalence (IQQ(153),IQBLN)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external NAUGHTD, LINER, DUNLIN, DARTER, MESHED, MASHED, HI, BYE
C
C               DP(N ,LDLMX), XNE(N), DV(N ,LDLMX), A(N ,LDLMX), DW(N),
      dimension DP(NR,*),     XNE(*), DV(NR,*),     A(NR,*),     DW(*),
C
C               ELL(N,KM), PHI(N,KM), U(N ,KM,LDLMX), PHC(N ,KM,LDLMX),
     $          ELL(*),    PHI(*),    U(NR,K,*),      PHC(NR,K,*),
C
C               CDL(LDLMX), VFI(N,KM), VFC(N ,KM,LDLMX), FDDL(N), Z(N),
     $          CDL(*),     VFI(*),    VFC(NR,K,*),      FDDL(*), Z(*),
C
C               DL(KM), DDL(LDLMX), VX(N)
     $          DL(*),  DDL(*),     VX(*)
C
      call HI ('BANANA')
C     !BEG
      call NAUGHTD    (VX, 1, NR, IVX)
      IVX = .not.IVX
C
      if(MPROM.le.0) then
        LAB = 'Voigt function'
      else if(MPROM.eq.1) then
        LAB = 'convolution of Voigt and Stark'
      end if
C
      if(LDL.le.1) then
        call DUNLIN   (NO, NR, IVX, .true., DW, DP, VX, DV, A, Z,
     $                 MPROM, XNE, IHSSP, FDDL)
        call LINER    (2, NO)
        write (NO,101) LAB
        call DARTER   (NO, NR, K, DL, Z, PHI)
        if(MPROM.le.0) then
          call LINER  (2, NO)
          write (NO,102)
          call DARTER (NO, NR, K, DL, Z, U)
        else if(MPROM.eq.1) then
          call LINER  (2, NO)
          write (NO,103)
          call DARTER (NO, NR, K, DL, Z, VFI)
          call LINER  (2, NO)
          write (NO,104)
          call DARTER (NO, NR, K, DL, Z, ELL)
        end if
C     !EJECT
      else
        if(IQBLN.gt.0) then
C
          call MESHED     ('BANANA', 2)
          do 105 I = 1,LDL
            call LINER    (2, LUEO)
            write (LUEO,100) I,DDL(I),CDL(I)
  100       format(' ','For ',I2,'. component: offset =',F8.4,
     $                 ', weight =',F8.4)
            call DUNLIN   (LUEO, NR, IVX, .true., DW, DP(1,I), VX,
     $                     DV(1,I), A(1,I), Z, MPROM, XNE, IHSSP, FDDL)
            call LINER    (2, LUEO)
            write (LUEO,101) LAB
  101       format(' ','Line Profile "PHI" = ',A)
            call DARTER   (LUEO, NR, K, DL, Z, PHC(1,1,I))
            if(MPROM.le.0) then
              call LINER  (2, LUEO)
              write (LUEO,102)
  102         format(' ','Voigt Function parameter "U"')
              call DARTER (LUEO, NR, K, DL, Z, U(1,1,I))
            else if(MPROM.eq.1) then
              call LINER  (2, LUEO)
              write (LUEO,103)
  103         format(' ','Voigt function without Stark broadening')
              call DARTER (LUEO, NR, K, DL, Z, VFC(1,1,I))
              call LINER  (2, LUEO)
              write (LUEO,104)
  104         format(' ','Stark profile')
              call DARTER (LUEO, NR, K, DL, Z, ELL)
            end if
  105     continue
          call MASHED     ('BANANA')
C
        else
          call DUNLIN     (NO, NR, .false., .false., DW, DP, dummy,
     $                     dummy, dummy, Z, MPROM, XNE, IHSSP, FDDL)
        end if
C
        call LINER        (2, NO)
        write (NO,106)
  106   format(' ','Composite Absorption Profile')
        call DARTER       (NO, NR, K, DL, Z, PHI)
C
      end if
C     !END
      call BYE ('BANANA')
C
      return
      end
