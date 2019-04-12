      subroutine ROALD
     $(SFT,SBI,QAR,KNT)
C
C     Rudolf Loeser, 1987 Dec 01
C---- Encodes SBI as Sobolev indices, when the corresponding SFT = 3.
C     !DASH
      save
C     !DASH
      real*8 SBI, SFT
      integer I, ISB, ISB1, ISB2, KNT, LSFT
      character BLANK*1, QAR*10
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external  HI, BYE
C
C               SFT(8), SBI(8), QAR(16)
      dimension SFT(*), SBI(*), QAR(*)
C
      call HI ('ROALD')
C     !BEG
      do 101 I = 1,KNT
        QAR(I) = BLANK
C
        LSFT = SFT(I)
        if(LSFT.eq.3) then
          ISB  = SBI(I)
          ISB1 = ISB/10000
          ISB2 = ISB-10000*ISB1
          write (QAR(I),100) ISB1,ISB2
  100     format(I5,',',I4)
        end if
C
  101 continue
C     !END
      call BYE ('ROALD')
C
      return
      end
