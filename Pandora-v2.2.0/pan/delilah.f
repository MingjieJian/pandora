      subroutine DELILAH
     $(N,NRAD,IMAGE,TAUIJ,TE,TLO,THI)
C
C     Rudolf Loeser, 1980 Oct 30
C---- Enters line segments into the plot image, for DITHER.
C     !DASH
      save
C     !DASH
      real*8 TAUIJ, TE, THI, TL, TLO, ZERO
      integer I, IE, IS, J, LINC, N, NRAD
      character IMAGE*(*), SYM*1
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
C     !DASH
      external GARLIC, GAZA, BEIGE, HI, BYE
C
C               TAUIJ(N,NRAD), TE(N)
      dimension TAUIJ(N,*),    TE(*)
C
      call HI ('DELILAH')
C     !BEG
      do 101 J = 1,NRAD
        call GAZA         (TAUIJ(1,J), N, TLO, THI, IS, IE)
        if(IE.gt.IS) then
          call GARLIC     (J, ALPHS, 26, SYM)
          LINC = 1
          do 100 I = IS,IE
            if(TAUIJ(I,J).gt.ZERO) then
              TL = log10(TAUIJ(I,J))
              call BEIGE  (IMAGE, TL, TE(I), SYM, LINC)
            end if
  100     continue
        end if
  101 continue
C     !END
      call BYE ('DELILAH')
C
      return
      end
