      subroutine FORE
     $(IMAGE,ZIN,ZAX,TS,M,TAUIJ,N,NT)
C
C     Rudolf Loeser, 1968 Jul 31
C---- Constructs the Tau-scale graph itself, for ONE.
C     !DASH
      save
C     !DASH
      real*8 FL, HALF, ONE, TAUIJ, TS, ZAX, ZERO, ZIN
      integer I, IL, IU, IUL, IY, J, JX, KLIN, LINE, M, N, NH, NT
      logical GOOD
      character BLANK*1, IMAGE*(*), NUMERO*1
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
      equivalence (LINKDS( 3),KLIN )
C     !EJECT
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 2),ONE   )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
      equivalence (SYMBS(38),NUMERO)
C     !DASH
      external KINIT, KWHERE, KPLOTP, INTRANS, KRIGIA, PET, HI, BYE
C
C               TS(M), TAUIJ(N,NT)
      dimension TS(*), TAUIJ(N,*)
C
      data NH /117/
C
      call HI ('FORE')
C     !BEG
      LINE = NT+1
      call KINIT        (IMAGE, ZIN, ZAX, ZERO, ONE, LINE, NH, BLANK,
     $                   GOOD)
      if(.not.GOOD) then
        call KRIGIA     (ZIN, ZAX, ZERO, ONE, LINE, NH)
      end if
      LINE = 1
      do 100 I = 2,M
        FL = log10(TS(I))
        call KWHERE     (IMAGE, FL, HALF, JX, IY)
        call KPLOTP     (IMAGE, JX, LINE, NUMERO)
  100 continue
      do 102 J = 1,NT
        call PET        (J)
        if(KLIN.eq.1) then
          call INTRANS  (IU, IL, 'FORE', IUL)
          LINE = LINE+1
          do 101 I = 2,N
            FL = log10(TAUIJ(I,IUL))
            call KWHERE (IMAGE, FL, HALF, JX, IY)
            call KPLOTP (IMAGE, JX, LINE, NUMERO)
  101     continue
        end if
  102 continue
C     !END
      call BYE ('FORE')
C
      return
      end
