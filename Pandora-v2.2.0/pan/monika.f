      subroutine MONIKA
     $(KODE,QIONM,NC)
C
C     Rudolf Loeser, 2005 Sep 07
C---- Constructs the full name of the ion-of-the-run, and its length.
C     The input parameter KODE controls the format of the name --
C
C     KODE = 1: with dash and Roman numeral
C          = 2: with adjoining Arabic numeral
C
C     !DASH
      save
C     !DASH
      integer IONST, JC, KODE, NC
      logical HYDR
      character BLANK*1, QELSM*8, QIONM*8
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 56),IONST)
      equivalence (QZQ(  2),QELSM)
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C
C---- ROMAN       as of 1984 Apr 24
      character   ROMAN*5
      dimension   ROMAN(21)
      common      /ROMAN/ ROMAN
C     Roman numerals.
C     .
C     !DASH
C     !EJECT
      external HI, BYE
C
      call HI ('MONIKA')
C     !BEG
      if(QELSM(2:2).ne.BLANK) then
        JC = 2
      else
        JC = 1
      end if
C
      HYDR = QELSM(:3).eq.'H  '
C
      if(KODE.eq.1) then
        if(HYDR) then
          QIONM = 'H'
          NC    = 1
        else
          QIONM = QELSM(:JC)//'-'//ROMAN(IONST)
C
          NC = 8
  100     continue
            if(QIONM(NC:NC).eq.BLANK) then
              NC = NC-1
              goto 100
            end if
        end if
C
      else
C
        if(HYDR) then
          QIONM = 'H1'
          NC    = 2
        else
          QIONM = QELSM
          if(IONST.gt.9) then
            write (QIONM(JC+1:),101) IONST
  101       format(I2)
            NC = JC+2
          else
            write (QIONM(JC+1:),102) IONST
  102       format(I1)
            NC = JC+1
          end if
        end if
      end if
C     !END
      call BYE ('MONIKA')
C
      return
      end
