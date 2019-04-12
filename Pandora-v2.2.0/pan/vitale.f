      subroutine VITALE
     $(ALF,BATA,BDJ,S,SSTAR,N,NL,NT,MRHO)
C
C     Rudolf Loeser, 1996 Jan 31
C---- Computes S* for radiative transitions, for TULIP.
C     (This is version 2 of VITALE.)
C     !DASH
      save
C     !DASH
      real*8 ALF, ALPHA, BATA, BDJ, BDJIT, ONE, R, S, SSTAR, ZERO
      integer I, IL, IU, IUL, J, JUL, KLIN, MRHO, N, NL, NT
      logical KILROY
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
      equivalence (LINKDS( 3),KLIN )
C     !DASH
C     !EJECT
      external MOVE1, PET, INDXUL, INTRANS, BRAT, DIVIDE, TWICK, MASHED,
     $         HI, BYE
C
C               ALF(MUL), BATA(N,MUL), BDJ(N,NL), SSTAR(N,NT), S(N,NT)
      dimension ALF(*),   BATA(N,*),   BDJ(*),    SSTAR(N,*),  S(*)
C
      call HI ('VITALE')
C     !BEG
      call MOVE1          (S, (N*NT), SSTAR)
C
      KILROY = .true.
      do 101 J = 1,NT
        call PET          (J)
        if(KLIN.eq.1) then
          call INTRANS    (IU, IL, 'VITALE', JUL)
          call INDXUL     (IU, IL, IUL)
          ALPHA = ALF(IUL)
C
          do 100 I = 1,N
            call BRAT     (I, IU, IL, BDJ, BDJIT)
C
            if(BDJIT.gt.ZERO) then
              call DIVIDE (ONE, (BDJIT*BATA(I,IUL)), R)
              call DIVIDE (ALPHA, (R-ONE), SSTAR(I,JUL))
C
            else
C             Error
              call TWICK  (KILROY, 'VITALE', MRHO, IU, IL, I, BDJIT)
              if(MRHO.eq.1) then
                MRHO = 0
              end if
            end if
C
  100     continue
C
        end if
  101 continue
      if(.not.KILROY) then
        call MASHED       ('VITALE')
      end if
C     !END
      call BYE ('VITALE')
C
      return
      end
