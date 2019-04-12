      subroutine AMULET
     $(NO,L,LDL,COMPUTE,LSAME,MPROM,F,DPM,C,Z,TE,XNE,XN1,HN1,DP,DW,
     $ TRD,TVW,TSK,TRS,TIB)
C
C     Rudolf Loeser, 1980 Aug 14
C---- Prints, for CHIMNEY.
C     (This is version 2 of AMULET.)
C     !DASH
      save
C     !DASH
      real*8 A, AF, BF, C, CF, DF, DIV, DP, DPM, DW, EF, F, HN1, PW, TE,
     $       TIB, TRD, TRS, TSK, TVW, XN1, XNE, Z
      integer I, IL, IU, L, LDL, LSAME, MPROM, N, NO
      logical COMPUTE
      character AH*1, BH*1, CH*1, DH*1, EH*1
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
C     !EJECT
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
      equivalence (RZQ(  7),PW   )
C     !DASH
      external ASP, GABBRO, DAGGER, DIVIDE, SHIM, HI, BYE
C
C               Z(N), TE(N), XNE(N), XN1(N), HN1(N), TIB(N), DP(N,LDL),
      dimension Z(*), TE(*), XNE(*), XN1(*), HN1(*), TIB(*), DP(*),
C
C               TRD(N), TSK(N), TRS(N), DW(N), TVW(N), C(5), F(5)
     $          TRD(*), TSK(*), TRS(*), DW(*), TVW(*), C(*), F(*)
C
      call HI ('AMULET')
C     !BEG
      if(NO.gt.0) then
        call ASP        (NO, L, IU, IL, PW, MPROM, F, DPM, LDL, LSAME,
     $                   COMPUTE)
        if(COMPUTE) then
C
          do 101 I = 1,N
            call GABBRO (TRD(I), TVW(I), TSK(I), TRS(I), TIB(I),
     $                   F, DPM, C, DIV)
            call DAGGER (AF, BF, CF, DF, EF, AH, BH, CH, DH, EH,
     $                   C(1), C(2), C(3), C(4), C(5), DIV)
            call DIVIDE (DP(I), DW(I), A)
C
            write (NO,100) I,Z(I),TE(I),XNE(I),XN1(I),HN1(I),DP(I),
     $                       AF,AH,BF,BH,CF,CH,DF,DH,EF,EH,A
  100       format(' ',I3,1X,1P2E12.4,1X,3E13.5,E15.6,1X,5(0PF5.2,A1),
     $                 1PE13.5)
            call SHIM   (I, 5, NO)
  101     continue
C
        end if
      end if
C     !END
      call BYE ('AMULET')
C
      return
      end
