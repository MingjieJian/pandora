      subroutine EMORY
     $(LU,X,PHI,ELL,PROD,M)
C
C     Rudolf Loeser, 1991 Dec 18
C---- Plots, for EMMA.
C     (This is version 2 of EMORY.)
C     !DASH
      save
C     !DASH
      real*4 ELL, PHI, PROD, X, YL, YU
      integer I, LU, M
      logical GOOD
      character NUMERO*1
C     !COM
C---- IMAGE       as of 1997 Aug 21
      integer     IMALEN
      parameter   (IMALEN=65535)
      character   IMAGE*(IMALEN)
      common      /IMAGE/ IMAGE
C     Character string to hold plot images constructed by the
C     K-type line printer plotting routines;
C     but used also as a general scratch character array.
C     .
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(38),NUMERO)
C     !DASH
C     !EJECT
      external OAT, SINIT, SPLOTC, ABJECT, KPRINT, HI, BYE
C
C               X(M), PHI(M), ELL(M), PROD(M)
      dimension X(*), PHI(*), ELL(*), PROD(*)
C
      call HI ('EMORY')
C     !BEG
      do 100 I = 1,M
        PROD(I) = PHI(I)*ELL(I)
  100 continue
C
      call OAT        (PHI, ELL, PROD, M, YL, YU)
      call SINIT      (IMAGE, X(1), X(M), YL, YU, 55, 117, NUMERO,
     $                 GOOD)
      if(GOOD) then
C
        do 101 I = 1,M
          call SPLOTC (IMAGE, X(I), log10(ELL(I)),  ALPHS(12))
          call SPLOTC (IMAGE, X(I), log10(PHI(I)),  ALPHS(16))
          call SPLOTC (IMAGE, X(I), log10(PROD(I)), ALPHS( 1))
  101   continue
C
        call ABJECT (LU)
        write (LU,102)
  102   format(' ',20X,'Plot of log10s of Voigt (= P), Ell (= L) and ',
     $             'Voigt*Ell (= A) vs. X')
        call KPRINT   (IMAGE, LU)
C
      end if
C     !END
      call BYE ('EMORY')
C
      return
      end
