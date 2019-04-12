      subroutine ESS
     $(I,EV,S,ELL,A,ELLE,ELLH,ELLHE,ELLHE2,XNE,XNP,XNH1,XNHE11,XNHEP,
     $ XNHE2K,DUMP)
C
C     Rudolf Loeser, 1984 May 08
C---- Computes S(I),
C     for the calculation of particle energy dissipation.
C     !DASH
      save
C     !DASH
      real*8 A, AI, ARG, BI, C1, C2, C3, C4, C5, C6, CI, CMPKM, DI, E,
     $       ELCHRG, ELL, ELLE, ELLH, ELLHE, ELLHE2, ELMASS, EV, FAC,
     $       FOUR, PI, PLANCK, RT, S, TWO, XNE, XNH1, XNHE11, XNHE2K,
     $       XNHEP, XNP, ZERO
      integer I, LUEO
      logical DUMP, KILROY
C     !COM
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (PCON(12),ELCHRG)
      equivalence (PCON( 6),ELMASS)
      equivalence (PCON( 1),PLANCK)
      equivalence (TUNI( 1),PI    )
      equivalence (TUNI( 5),CMPKM )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 3),TWO   )
      equivalence (DLIT( 5),FOUR  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external GROPE, LINER, HI, BYE
C
C               EV(N), S(N), A(N), ELLE(N), ELLH(N), ELLHE(N), XNH1(N),
      dimension EV(*), S(*), A(*), ELLE(*), ELLH(*), ELLHE(*), XNH1(*),
C
C               ELLHE2(N), XNP(N), XNHE11(N), XNHEP(N), XNHE2K(N),
     $          ELLHE2(*), XNP(*), XNHE11(*), XNHEP(*), XNHE2K(*),
C
C               XNE(N)
     $          XNE(*)
C
      data KILROY,FAC /.true., 1.8D0/
C
      call HI ('ESS')
C     !BEG
      if(KILROY) then
        KILROY   =.false.
C----   Initialize
        C1 = TWO/(ELCHRG**2)
        C2 = (TWO*PI)/PLANCK
        C3 = TWO/ELMASS
        C4 = FAC*ELL
        C5 = FOUR*ELL
        C6 = FOUR*PI*(ELCHRG**4)*CMPKM
      end if
C
      E = EV(I)
C
C---- Compute LE and LH
      if(E.lt.ELL) then
        ARG = C1*A(I)*E
        ELLE(I) = log(ARG)
        ELLH(I) = ZERO
      else
        ARG = C3*E
        RT  = sqrt(ARG)
        ARG = C2*A(I)*RT
        ELLE(I) = log(ARG)
        ELLH(I) = log(E/ELL)
      end if
C
C---- Compute LHE
      if(E.lt.C4) then
        ELLHE(I) = ZERO
      else
        ELLHE(I) = log(E/C4)
      end if
C
C---- Compute LHE2
      if(E.lt.C5) then
        ELLHE2(I) = ZERO
      else
        ELLHE2(I) = log(E/C5)
      end if
C     !EJECT
C---- Compute AI, BI, CI, DI
      call GROPE   (XNH1(I), ELLH(I), XNHE11(I), ELLHE(I), XNHEP(I),
     $              ELLHE2(I), XNE(I), XNP(I), XNHE2K(I), ELLE(I),
     $              AI, BI, CI, DI)
C
C---- Compute S
      S(I) = C6*(AI+BI+CI+DI)
C
      if(DUMP) then
C----   Dump
        call LINER (1, LUEO)
        write (LUEO,100) I,S(I),E,ELLH(I),AI,ELLHE(I),BI,ELLHE2(I),CI,
     $                  ELLE(I),DI
  100   format(' ','S(',I2,') =',1PE15.7,', using E =',E15.7/
     $         ' ','    LH =',E15.7,'  A =',E15.7/
     $         ' ','   LHE =',E15.7,'  B =',E15.7/
     $         ' ','  LHE2 =',E15.7,'  C =',E15.7/
     $         ' ','    LE =',E15.7,'  D =',E15.7)
      end if
C     !END
      call BYE ('ESS')
C
      return
      end
