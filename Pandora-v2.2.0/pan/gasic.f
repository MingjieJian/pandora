      subroutine GASIC
     $(N,IMAGE,KOUNT,ZL,IBEG,IEND, XNE,XNP,ZHEL,R,RL)
C
C     Rudolf Loeser, 1998 Aug 19
C---- Adds Hydrogen and Helium data to the FMC graph.
C     !DASH
      save
C     !DASH
      real*8 R, RL, TEN, XNE, XNP, ZHEL, ZL
      integer IBEG, IEND, KOUNT, KOUNTA, KOUNTB, N
      character IMAGE*(*), PLUS*1, STAR*1
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(11),TEN   )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(39),PLUS  )
      equivalence (SYMBS(45),STAR  )
C     !DASH
      external KKOUNT, SHRIMP, LETUP, ARRDIV, HI, BYE
C
C               ZL(N), XNE(N), XNP(N), ZHEL(N), R(N), RL(N)
      dimension ZL(*), XNE(*), XNP(*), ZHEL(*), R(N), RL(*)
C
      call HI ('GASIC')
C     !BEG
      call KKOUNT (IMAGE,KOUNTA)
C
      call ARRDIV (XNP ,XNE,R,N)
      call LETUP  (R,RL,N,1,TEN)
      call SHRIMP (ZL,N,IBEG,IEND, RL,1,PLUS,1,TEN,2,IMAGE)
C
      call ARRDIV (ZHEL,XNE,R,N)
      call LETUP  (R,RL,N,1,TEN)
      call SHRIMP (ZL,N,IBEG,IEND, RL,1,STAR,1,TEN,2,IMAGE)
C
      call KKOUNT (IMAGE,KOUNTB)
      KOUNT = KOUNT+(KOUNTB-KOUNTA)
C     !END
      call BYE ('GASIC')
C
      return
      end
