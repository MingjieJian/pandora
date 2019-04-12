      subroutine FALKE
     $(LU,N,K,DP,DW,SAP,GMA,DRLIMI,XI,DL,A,XJNU,XJBR,PHI,FAB,FJN,FJJ,
     $ GJN,XK1,XK2,DJB)
C
C     Rudolf Loeser, 2005 Jan 14
C---- Prints PRD quantities.
C     !DASH
      save
C     !DASH
      real*8 A, DJB, DL, DP, DRLIMI, DW, FAB, FJJ, FJN, GJN, GMA, PHI,
     $       SAP, XI, XJBR, XJNU, XK1, XK2
      integer I, IPRDD, J, K, LDL, LU, N
      logical PRINT
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
      equivalence (LINKDS(12),LDL  )
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 98),IPRDD)
C     !DASH
C     !EJECT
      external VITIM, LINER, SEKLA, HI, BYE
C
C               SAP(N,K), A(K), DRLIMI(N), DW(N), DP(N,LDL), XJNU(N,K),
      dimension SAP(*),   A(*), DRLIMI(*), DW(*), DP(*),     XJNU(N,*),
C
C               FJN(N,K), XJBR(N), XK1(N,K), XK2(N,K), DL(K), 
     $          FJN(N,*), XJBR(*), XK1(N,*), XK2(N,*), DL(*), 
C
C               XI(K), GJN(N,K), GMA(N), PHI(N,K), FAB(N,K), FJJ(N,K),
     $          XI(*), GJN(N,*), GMA(*), PHI(N,*), FAB(N,*), FJJ(N,*),
C
C               DJB(N,K)
     $          DJB(N,*)
C
      call HI ('FALKE')
C     !BEG
      if(LU.gt.0) then
C----   Write stuff that does not vary with frequency
        call SEKLA     (LU, N, LDL, DP, SAP, DW, GMA, DRLIMI, XJBR)
        call LINER     (2, LU)
C
C----   Write stuff that varies with frequency too
        do 103 J = 1,K
          call VITIM   (LU, 1, K, J, DL, XI, A, PRINT)
          if(PRINT) then
            call LINER (1, LU)
            write (LU,100)
  100       format(' ',4X,10X,'JNU',10X,'PHI',10X,'FAB',10X,'FJJ',
     $                 10X,'FJN',10X,'GJN',10X,'DJB',11X,'K1',
     $                 11X,'K2')
            call LINER (1, LU)
            do 102 I = 1,N,IPRDD
              write (LU,101) I,XJNU(I,J),PHI(I,J),FAB(I,J),FJJ(I,J),
     $                         FJN(I,J),GJN(I,J),DJB(I,J),XK1(I,J),
     $                         XK2(I,J)
  101         format(' ',I4,1P9E13.5)
  102       continue
          end if
  103   continue
      end if
C     !END
      call BYE ('FALKE')
C
      return
      end
