      subroutine HAREM
     $(X,IX,NO,CTE,CNC,HYDR)
C
C     Rudolf Loeser, 2006 Apr 12
C---- Prints CI/CE information.
C     (This is version 2 of HAREM.)
C     !DASH
      save
C     !DASH
      real*8 CNC, CTE, FROSC, X
      integer IX, MCEOF, MCIOF, NO
      logical HYDR
C     !COM
C---- SUBLET      as of 2006 Dec 04
      character   CITES*64, CQ*20
      dimension   CITES(11), CQ(11)
      common      /SUBLET/ CITES, CQ
C     CI/CE methods source citations for HAMRE et al.
C     .
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(224),MCIOF)
      equivalence (KZQ(223),MCEOF)
      equivalence (RZQ(129),FROSC)
C     !DASH
C     !EJECT
      external LINER, KONYA, IZMIR, SIVAS, ADANA, HI, BYE
C
      dimension X(*), IX(*)
C
      call HI ('HAREM')
C     !BEG
      call LINER   (1, NO)
      write (NO,100) 'CI'
  100 format(' ','The following methods are available for ',A,':')
      call LINER   (1, NO)
      write (NO,101) 'SHAH',CITES(1),CQ(1),'AR',CITES(2),CQ(2),
     $               'VORONOV',CITES(3),CQ(3),'VS',CITES(4),CQ(4),
     $               'JOHNSON',CITES(5),CQ(5),'CLARK',CITES(6),CQ(6)
  101 format(' ',15X,A10,5X,A64,2X,A20)
      call LINER   (1,NO)
      write (NO, 102) 'CIMETHOD'
  102 format(' ','The keyword on the left is used to select the ',
     $           'associated method with a ',A,' input statement.'/
     $       ' ','Any method(s) actually used in this run are ',
     $           'listed in Section ATOM, below.')
      call LINER   (1, NO)
      if(MCIOF.gt.0) then
        write (NO,103) 'CI'
  103   format(' ','This run computes ',A,'-values on-the-fly as ',
     $             'needed.')
      else
        write (NO,104) 'CI'
  104   format(' ','This run computes ',A,'-values as needed by ',
     $             'interpolation in the tables printed in ',
     $             'Section ATOM, below.')
      end if
C
      if(HYDR) then
        call KONYA (X, IX, NO, CTE, CNC)
      else
        call SIVAS (X, IX, NO, CTE, CNC)
      end if
C     !EJECT
      call LINER   (2, NO)
      write (NO,100) 'CE'
      call LINER   (1, NO)
      write (NO,101) 'SEATON',CITES(9),CQ(9),'VREGE',CITES(10),CQ(10),
     $               'VS',CITES(4),CQ(4),'JOHNSON',CITES(5),CQ(5),
     $               'AGGRWL',CITES(11),CQ(11),'PB',CITES(8),CQ(8),
     $               'SCHOLZ',CITES(7),CQ(7)
      call LINER   (1,NO)
      write (NO,102) 'CEMETHOD'
      write (NO,105) FROSC
  105 format(' ','(For A=0 transitions, VREGE uses A-values computed ',
     $           'with fraction-of-classical-oscillator-strength '/
     $       ' ','FROSCE =',1PE12.4,'. Such values are printed in ',
     $           '[brackets] in Section ATOM, below.)')
      call LINER   (1, NO)
      if(MCEOF.gt.0) then
        write (NO,103) 'CE'
      else
        write (NO,104) 'CE'
      end if
C
      if(HYDR) then
        call IZMIR (X, IX, NO, CTE, CNC)
      else
        call ADANA (X, IX, NO, CTE, CNC)
      end if
C     !END
      call BYE ('HAREM')
C
      return
      end
