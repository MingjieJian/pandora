      subroutine CRICK
     $(ARR,QAR,KNT)
C
C     Rudolf Loeser, 1980 Dec 28
C---- Converts SEM into Statistical Equilibrium method description,
C     for LIZARD.
C     (This is version 3 of CRICK.)
C     !DASH
      save
C     !DASH
      real*8 ARR
      integer I, ISEMD, KNT
      character BLANK*1, QAR*10, ZET*10
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external HI, BYE
C
C               ARR(8), QAR(16)
      dimension ARR(*), QAR(*)
C
      dimension ZET(6)
C
      data ZET /'  (unused)', '      Nova', ' COMPLEX-U', ' COMPLEX-L',
     $          '     Chain', '     Vamos'/
C
      call HI ('CRICK')
C     !BEG
      do 100 I = 1,KNT
        QAR(I) = BLANK
        ISEMD  = ARR(I)
        if((ISEMD.ge.1).and.(ISEMD.le.6)) then
          QAR(I) = ZET(ISEMD)
        else if(I.gt.1) then
          QAR(I) = ZET(1)
        end if
  100 continue
C     !END
      call BYE ('CRICK')
C
      return
      end
