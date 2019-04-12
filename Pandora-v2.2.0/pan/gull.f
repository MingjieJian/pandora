      subroutine GULL
     $(LIM,N,H,F,R1N,ISB1,ISB2,SBFEQ,SBDMX,SBDMN,Z,VSB,FXI,DV,RV)
C
C     Rudolf Loeser, 1986 Jul 30
C---- Dumps, for SKUA.
C     !DASH
      save
C     !DASH
      real*8 DV, F, FXI, H, R1N, RV, SBDMN, SBDMX, SBFEQ, VSB, X, Z
      integer I, IL, ISB1, ISB2, IU, J, LIM, LUEO, N
      character BLANK*1, Q*1
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
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
C     !EJECT
      external ABJECT, LINER, ANOA, HI, BYE
C
C               Z(N), VSB(N), DV(N), RV(N), FXI(N), H(LIM,9), F(LIM,9)
      dimension Z(*), VSB(*), DV(*), RV(*), FXI(*), H(LIM,*), F(LIM,*)
C
      dimension X(9)
C
      call HI ('GULL')
C     !BEG
      call ABJECT (LUEO)
      write (LUEO,100) IU,IL,ISB1,ISB2,R1N
  100 format(' ','Details of Sobolev escape probability calculation ',
     $           'for transition (',I2,'/',I2,').'/
     $       ' ','(This printout controlled by option SOBDUMP.)'//
     $       ' ','ISB1 =',I4,5X,'ISB2 =',I4,20X,'R1N =',1PE16.8//
     $       ' ',20X,'Z',13X,'VSB',14X,'DV',14X,'RV',13X,'FXI')
      write (LUEO,101) (I,Z(I),VSB(I),DV(I),RV(I),FXI(I),I=1,N)
  101 format(5(' ',I5,1P5E16.8/))
C
      call LINER  (3, LUEO)
      write (LUEO,102) SBFEQ,SBDMX,SBDMN
  102 format(' ','Integration controls:  SOBFEQ =',1PE10.2,5X,
     $           'SOBDMX =',E10.2,5X,'SOBDMN =',E10.2//
     $       ' ','Sample tables of X, H and F follow.')
      call ANOA   (LIM, FXI, DV, RV, X, H, F)
C
      call LINER  (1, LUEO)
      write (LUEO,103) (X(J),J=1,9)
  103 format(' ','X',5X,9F12.3)
C
      call LINER  (1, LUEO)
      Q = 'H'
      do 105 I = 1,LIM
        write (LUEO,104) Q,I,(H(I,J),J=1,9)
  104   format(' ',A1,I5,1P9E12.4)
        Q = BLANK
  105 continue
C
      call LINER  (1, LUEO)
      Q = 'F'
      do 106 I = 1,LIM
        write (LUEO,104) Q,I,(F(I,J),J=1,9)
        Q = BLANK
  106 continue
C     !END
      call BYE ('GULL')
C
      return
      end
