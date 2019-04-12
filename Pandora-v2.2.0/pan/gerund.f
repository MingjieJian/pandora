      subroutine GERUND
     $(N,H1,CLN,ROOT,AN1,AN1S,STKFN,X,A,PHI,OPAC,SCAT)
C
C     Rudolf Loeser, 2002 Sep 18
C---- Computes background opacity terms due to the cores of the
C     Hydrogen Lyman N/1 lines.
C     !DASH
      save
C     !DASH
      real*8 A, AN1, AN1S, CL3, CLN, DIVA, DIVO, EIGHT, EN2, ENN, FACT,
     $       FOUR, H1, OPAC, PHI, PI, ROOT, SCAT, STKFN, T, THREE, TWO,
     $       X, ZERO
      integer N
      logical KILROY
C     !COM
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (TUNI( 1),PI    )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 3),TWO   )
      equivalence (DLIT( 4),THREE )
      equivalence (DLIT( 5),FOUR  )
      equivalence (DLIT( 9),EIGHT )
C     !DASH
      external DVOIGT, HI, BYE
C
      data KILROY /.true./
C     !EJECT
C
      call HI ('GERUND')
C     !BEG
      if(KILROY) then
        KILROY = .false.
        DIVA = FOUR*PI
        DIVO = EIGHT*PI
        FACT = THREE/(EIGHT*(PI**2))
      end if
C
      ENN = N
      EN2 = N**2
      CL3 = CLN**3
C
      T = FACT*AN1*CL3*ENN*H1
      A = (((AN1S+T)/DIVA)+STKFN)/(ROOT/CLN)
C
      call DVOIGT (X, A, PHI)
C
      OPAC = ((EN2*AN1)/DIVO)*(CL3/ROOT)*PHI*H1
      SCAT = ZERO
C     !END
      call BYE ('GERUND')
C
      return
      end
