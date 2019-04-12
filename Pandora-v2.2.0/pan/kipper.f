      subroutine KIPPER
     $(E,L,K)
C
C     Rudolf Loeser, 1992 Dec 24
C---- Sets up markers for ITURI.
C     (This is version 5 of KIPPER.)
C     !DASH
      save
C     !DASH
      real*8 E, ZERO
      integer I, K
      character BLANK*1, L*2, M*2, STAR*1
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
      equivalence (SYMBS(43),BLANK )
      equivalence (SYMBS(45),STAR  )
C     !DASH
      external SETC, HI, BYE
C
C               E(K), L(K)
      dimension E(*), L(*)
C
      call HI ('KIPPER')
C     !BEG
      call SETC (L,1,K,BLANK)
C
      M = STAR//STAR
C
      do 100 I = 1,K
        if(E(I).lt.ZERO) then
          L(I) = M
        end if
  100 continue
C     !END
      call BYE ('KIPPER')
C
      return
      end
