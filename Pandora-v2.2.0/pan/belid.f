      subroutine BELID
     $(NO)
C
C     Rudolf Loeser, 1999 Jan 07
C---- Prints GVL-related stuff, for PATCH.
C     !DASH
      save
C     !DASH
      real*8 CLVLS
      integer I, IPIJG, KDFGA, KDFGB, KDFGS, MDFG, MNG1, NGNV, NO
      character LAB*1
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
      equivalence (KZQ(110),MNG1 )
      equivalence (RZQ(134),CLVLS)
      equivalence (KZQ(154),KDFGS)
      equivalence (KZQ(155),KDFGA)
      equivalence (KZQ(156),KDFGB)
      equivalence (KZQ(172),MDFG )
      equivalence (KZQ(205),NGNV )
      equivalence (KZQ(206),IPIJG)
C     !DASH
      external  LINER, NIBBLE, HI, BYE
C
      dimension LAB(3)
C     !EJECT
C
      call HI ('BELID')
C     !BEG
      call NIBBLE (LAB, (KDFGS+1), 1, 3)
      write (NO,100) MNG1,(LAB(I),I=1,3),KDFGA,KDFGB
  100 format(' ','Limit for GNV-1 replacement: MNG1 =',I5//
     $       ' ','Reducing the computed values of GNVL by the ',
     $           'multiplier GRF(i) is controlled by KDIFGS:'/
     $       ' ','  KDIFGS    =  0',A,', no reduction'/
     $       ' ','            =  1',A,', reduce GNV-1 only'/
     $       ' ','            =  2',A,', reduce all GNVL'/
     $       ' ','     GRF(i) is generated using the depth indices ',
     $           'KDIFGA =',I4,', and KDIFGB =',I4)
      call NIBBLE (LAB, (NGNV+1), 1, 2)
      call LINER  (1, NO)
      write (NO,101) NGNV,(LAB(I),I=1,2)
  101 format(' ','GNV-suppression is controlled by NGNV =',I3,':'/
     $       ' ','  NGNV      =  0',A,', no suppression'/
     $       ' ','            = >0',A,', they are suppressed for ',
     $           'levels NGNV to NL, inclusive')
      call NIBBLE (LAB, (IPIJG+1), 1, 2)
      call LINER  (1, NO)
      write (NO,102) (LAB(I),I=1,2)
  102 format(' ','Fudging the use of GNV in calculating PIJ is ',
     $           'controlled by IPIJG:'/
     $       ' ','  IPIJG     =  0',A,', do not fudge'/
     $       ' ','            =  1',A,', fudge (to prevent negative ',
     $           'values of b or N)')
      call LINER  (1, NO)
      write (NO,103) CLVLS
  103 format(' ','Parameter for GH-L: CLEVELS =',1PE12.4)
      call NIBBLE (LAB, (MDFG+1), 1, 2)
      call LINER  (1, NO)
      write (NO,104) (LAB(I),I=1,2)
  104 format(' ','Writing the computed GNV-values to an output file ',
     $           'is controlled by MDFG:'/
     $       ' ','  MDFG      =  0',A,', they are not saved'/
     $       ' ','            =  1',A,', they are saved')
C     !END
      call BYE ('BELID')
C
      return
      end
