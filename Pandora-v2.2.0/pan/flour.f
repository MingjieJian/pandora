      subroutine FLOUR
     $(WN,FVAL,WNX)
C
C     Rudolf Loeser, 1992 Sep 17
C---- Computes a CO-opacity contribution.
C     !DASH
      save
C     !DASH
      real*8 A, EXT, FACJ, FACK, FJ, FVAL, G, ONE, PROF, ROOTPI, TWO,
     $       WN, WNX, XFAC, ZERO
C     !COM
C---- MOLONGA     as of 2003 Dec 02
      integer     I,JUD,L,KOD,M
      real*8      T,H,C1,C2,C3,C4,AX,SHFT,WLP,WLM,ABISO,ET,SUM
      logical     DPL,DMP,STT
C
      dimension   C1(2),C3(2),C4(2),WLP(2),WLM(2)
      common      /MOLONG1/ T,H,C1,C2,C3,C4,AX,SHFT,WLP,WLM,ABISO,
     $                      ET,SUM
      common      /MOLONG2/ I,JUD,L,KOD,M
      common      /MOLONG3/ DPL,DMP,STT
C     Intermediates for CO-opacity calculation.
C     .
C---- ARABIS      as of 1989 Feb 22
      integer     NCOOP,NCOPR
      real*8      COOTM
      common      /ARABIS1/ NCOOP,NCOPR
      common      /ARABIS2/ COOTM
C     NCOOP = number of "bona fide" CO opacity values requested;
C     NCOPR = number of Voigt profile evaluations for CO opacity;
C     COOTM = total time (sec) for "bona fide" values.
C     .
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
C
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (TUNI( 2),ROOTPI)
C     !DASH
C     !EJECT
      external PVOIGT, WELLA, HALT, HI, BYE
C
      data FACK,FACJ /3.1219D3, 2.778D0/
C
      call HI ('FLOUR')
C     !BEG
      if(DPL) then
        EXT  = exp(-(AX**2))
        PROF = EXT/ROOTPI
      else
        call PVOIGT  (AX, C3(M), PROF)
        if(STT) then
          NCOPR = NCOPR+1
        end if
      end if
C
      G = C4(M)*(PROF/WN)
      if(G.ne.ZERO) then
        EXT = exp(-(ET*WNX))
        if(JUD.eq.+1) then
          FJ = 2*I-1
        else if(JUD.eq.-1) then
          FJ = 2*I+1
        else
          write (MSSLIN(1),100) JUD
  100     format('JUD =',I12,', which is neither -1 nor +1.')
          call HALT  ('FLOUR', 1)
        end if
        XFAC = FJ*EXT
C
        A = ((FVAL/ABISO)*XFAC)*G*H
C
        SUM = SUM+A
C
        if(DMP) then
          call WELLA (G, A, PROF, WNX, EXT, XFAC)
        end if
C
      end if
C     !END
      call BYE ('FLOUR')
C
      return
      end
