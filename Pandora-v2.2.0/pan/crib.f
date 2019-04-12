      subroutine CRIB
     $(LCX,NPQ,LRQ,IB,IE,QAR,BRR,M)
C
C     Rudolf Loeser, 1990 Nov 21
C---- Encodes charge exchange code, and selects RCHX values.
C     !DASH
      save
C     !DASH
      real*8 BRR, ZERO
      integer I, IB, IE, LCX, LRQ, M, NPQ
      character BLANK*1, QAR*10, YES*10
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- XINGU       as of 1999 Sep 21
      real*8      AXED,BXED,RCHX,DELCHX
      character   NAMXED*3
      integer     NXI,NPQLM,NPQMX
      parameter   (NXI=10)
C     (Remember to change all users when changing NXI)
      parameter   (NPQLM=15)
C     (Maximum permitted value of principal quantum number n)
C     (NPQLM must not exceed LIMDAT(1) [in popdata.inc], the
C     number of levels in the Hydrogen population ion model.)
      dimension   AXED(NXI), BXED(NXI), NAMXED(NXI)
      dimension   RCHX(NPQLM,NPQLM), DELCHX(NPQLM,NPQLM)
      common      /XINGU1/ AXED,BXED,RCHX,DELCHX
      common      /XINGU2/ NAMXED
      common      /XINGU3/ NPQMX
C---- Charge Exchange data tables
C     .
C     !DASH
      external HI, BYE
C
C               LCX(NL), NPQ(NL), LRQ(NL), BRR(8), QAR(16)
      dimension LCX(*),  NPQ(*),  LRQ(*),  BRR(*), QAR(*)
C
      data YES /'       yes'/
C     !EJECT
C
      call HI ('CRIB')
C     !BEG
      QAR(1) = BLANK
      BRR(1) = ZERO
C
      M = 1
      do 100 I = IB,IE
        M = M+1
        if(LCX(I).le.0) then
          QAR(M) = BLANK
          BRR(M) = ZERO
        else
          QAR(M) = YES
          BRR(M) = RCHX(NPQ(I),LRQ(I))
        end if
  100 continue
C     !END
      call BYE ('CRIB')
C
      return
      end
