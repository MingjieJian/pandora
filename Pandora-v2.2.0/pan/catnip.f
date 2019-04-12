      subroutine CATNIP
     $(INDX,XLM,N,NOPAC,HNK,HN,TE,CONT)
C
C     Rudolf Loeser, 1988 Oct 27
C---- Computes a set of H2+ opacity values.
C     !DASH
      save
C     !DASH
      real*8 A1, A2, A3, A4, A5, AP, AR, AS, B, B1, B2, B3, B4, B5, B6,
     $       C, C1, C2, C3, C4, CONT, D, E, EUP, EUS, FR, HN, HNK, RW,
     $       RYDBRG, TE, TEN, UP, US, X, XLM, ZERO
      integer I, INDX, N, NOPAC
      logical YES
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(11),TEN   )
C
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (PCON( 4),RYDBRG)
C     !DASH
C     !EJECT
      external WITHIN, ZEROD, HI, BYE
C
C               HNK(N), HN(N,Limp), TE(N), CONT(Nopac,N)
      dimension HNK(*), HN(N,*),    TE(*), CONT(NOPAC,*)
C
      data B,C,D /1.D4, 6.334D-6, 1.D-26/
C
      data A1,A2,A3,A4,A5 /
     $ -1.4966168D1, -2.420202D0, -1.105547D0, -3.162133D-1,
     $ -3.76148D-2/
C
      data B1,B2,B3,B4,B5,B6 /
     $ -6.923734D-1, -9.884469D-2, -1.4031698D0, -1.041392D0,
     $ -3.951385D-1, -5.921215D-2/
C
      data C1,C2,C3,C4 /
     $ -1.014162D-1, 1.298161D0, 1.095814D-1, 2.2345D-2/
C
      call HI ('CATNIP')
C     !BEG
      call WITHIN   (RYDBRG, XLM, B, 1, YES)
      if(YES) then
C
        RW = RYDBRG/XLM
        X  = log10(RW)
        AR = (((A5*X+A4)*X+A3)*X+A2)*X+A1
        FR = TEN**AR
        AS = ((((B6*X+B5)*X+B4)*X+B3)*X+B2)*X+B1
        US = TEN**AS
        AP = ((C4*X+C3)*X+C2)*X+C1
        UP = TEN**AP
C
        do 100 I = 1,N
          E   = C*TE(I)
          EUS = exp( US/E)
          EUP = exp(-UP/E)
C
          CONT(INDX,I) = (HNK(I)*HN(I,1)*D)*FR*(EUS-EUP)
  100   continue
C
      else
        call ZEROD  (CONT(INDX,1), NOPAC, N)
      end if
C     !END
      call BYE ('CATNIP')
C
      return
      end
