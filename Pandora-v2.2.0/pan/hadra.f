      subroutine HADRA
     $(FABD)
C
C     Rudolf Loeser, 1983 Mar 21
C---- Massages element abundances.
C     (This is version 3 of HADRA.)
C     !DASH
      save
C     !DASH
      real*8 FABD, ONE, TEN, TWELVE, ZERO
      integer I
C     !COM
C---- ELEMENT     as of 1998 Aug 17
      integer     NELX
      parameter   (NELX=50)
C     (Remember to recompile all users when changing NELX)
      real*8      ELABD,ELCHI,ELLU1,ELLU2,ELABL,ELDEF
      integer     LATNO,LDEFR,NMT,NMTMAX
      logical     LATEM
      character   ELSYM*3, ELSUB*3
      dimension   ELSYM(NELX),ELSUB(NELX),ELABD(NELX),ELCHI(NELX),
     $            ELLU1(NELX),ELLU2(NELX),ELABL(NELX),ELDEF(NELX),
     $            LATNO(NELX),LDEFR(NELX),LATEM(NELX)
C
      common      /ELEMNT0/ NMT,NMTMAX
      common      /ELEMNT1/ ELSYM,ELSUB
      common      /ELEMNT2/ ELABD,ELCHI,ELLU1,ELLU2,ELABL,ELDEF
      common      /ELEMNT3/ LATNO,LDEFR
      common      /ELEMNT4/ LATEM
C
C     Element data tables:
C             ELSYM - element symbol;
C             ELSUB - (Scratch storage for I.D. symbols);
C             ELABD - abundance (w.r.t. Hydrogen);
C             ELCHI - Chi, i.e. ionization potential;
C             ELLU1 - U-I partition function;
C             ELLU2 - U-II partition function;
C             ELABL - logarithmic abundance;
C             ELDEF - defaults values of logarithmic abundance;
C             LATNO - atomic number; and
C             LDEFR - default values sources codes.
C             LATEM - "metal" designator
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT(11),TEN   )
C     !DASH
C     !EJECT
      external HI, BYE
C
      data TWELVE /1.2D1/
C
      call HI ('HADRA')
C     !BEG
C---- Set up logarithmic abundances.
      do 100 I = 1,NMT
        if(ELABD(I).gt.ZERO) then
          ELABL(I) = log10(ELABD(I))
          ELABL(I) = ELABL(I)+TWELVE
        end if
        if(ELABL(I).eq.ZERO) then
          ELABL(I) = ELDEF(I)
        end if
  100 continue
C
C---- Set up abundance ratios (and bona fide zero abundances)
      do 101 I = 1,NMT
        if(ELABD(I).eq.ZERO) then
          ELABD(I) = TEN**(ELABL(I)-TWELVE)
        else if(ELABD(I).lt.ZERO) then
          ELABD(I) = ZERO
          ELABL(I) = ZERO
        end if
  101 continue
C
      if(FABD.ne.ONE) then
C----   Apply abundance multiplier
        do 102 I = 1,NMT
          if((ELSYM(I).ne.'H  ').and.(ELSYM(I)(1:2).ne.'HE')) then
            ELABD(I) = FABD*ELABD(I)
            if(ELABD(I).gt.ZERO) then
              ELABL(I) = log10(ELABD(I))
              ELABL(I) = ELABL(I)+TWELVE
            end if
          end if
  102   continue
      end if
C     !END
      call BYE ('HADRA')
C
      return
      end
