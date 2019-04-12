      subroutine APATITE
     $(N,XND,P,RULED)
C
C     Rudolf Loeser, 1999 Nov 04
C---- Computes N(u)/N(l), for BOTTOM.
C     Also computes an edited version (for plotting).
C     (This is version 3 of APATITE.)
C     !DASH
      save
C     !DASH
      real*8 ONE, P, PRAT, RUL, RULED, XND, ZERO
      integer I, IL, IU, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
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
      external HI, BYE
C
C               XND(N,NL), RULED(N), P(NSL)
      dimension XND(N,*),  RULED(*), P(*)
C
      call HI ('APATITE')
C     !BEG
      PRAT = P(IL)/P(IU)
C
      do 100 I = 1,N
        if(XND(I,IL).le.ZERO) then
          RULED(I) = ONE
        else
          RUL = XND(I,IU)/XND(I,IL)
          if((RUL*PRAT).gt.ONE) then
            RULED(I) = ONE
          else
            RULED(I) = ZERO
          end if
        end if
  100 continue
C     !END
      call BYE ('APATITE')
C
      return
      end
