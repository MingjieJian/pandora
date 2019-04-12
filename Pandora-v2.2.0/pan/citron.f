      subroutine CITRON
     $(N,NL,AIJ,BATA,PE,FE,B,EP,EF,BS)
C
C     Rudolf Loeser, 1984 May 11
C---- Computes Statistical Equlibrium terms EP, EF and BS.
C     !DASH
      save
C     !DASH
      real*8 A, AIJ, B, BATA, BS, EF, EP, FBAT, FE, ONE, PE, PFBAT, RAT
      integer I, IL, IU, IUL, N, NL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
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
C     !DASH
C     !EJECT
      external INDXUL, DIVIDE, HI, BYE
C
C               AIJ(NL,NL), EP(N), EF(N), BS(N), BATA(N,MUL), PE(N),
      dimension AIJ(NL,*),  EP(*), EF(*), BS(*), BATA(N,*),   PE(*),
C
C               B(N), FE(N)
     $          B(*), FE(*)
C
      call HI ('CITRON')
C     !BEG
      call INDXUL   (IU, IL, IUL)
      A = AIJ(IU,IL)
C
      do 100 I = 1,N
        PFBAT = PE(I)-FE(I)*BATA(I,IUL)
        FBAT  = FE(I)*(ONE-BATA(I,IUL))
        EP(I) = PFBAT/A
        EF(I) = FBAT/A
        call DIVIDE (FBAT, PFBAT, RAT)
        BS(I) = B(I)*RAT
  100 continue
C     !END
      call BYE ('CITRON')
C
      return
      end
