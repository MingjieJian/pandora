      subroutine SEDUM
     $(INAME,INDEX,LCEX)
C
C     Rudolf Loeser, 2004 Sep 17
C---- Sets up ion name and indices for lower-level charge exchange.
C     Returns INDEX = 0 if this calculation does not apply.
C     (This is version 3 of SEDUM.)
C     !DASH
      save
C     !DASH
      integer INDEX, IONST, K, LCEX, LOOK, LUEO, NT
      character BLANK*1, INAME*3, QELSM*8, TAB*3
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
      equivalence (QZQ(  2),QELSM)
      equivalence (KZQ( 56),IONST)
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external LOOKUC, MESHED, MASHED, HI, BYE
C
      parameter (NT=27)
      dimension TAB(NT)
C
      data TAB /
     $ 'HE1', 'HE2', 'C1 ', 'C2 ', 'C3 ', 'C4 ', 'N1 ', 'N2 ', 'N3 ',
     $ 'N4 ', 'O1 ', 'O2 ', 'O3 ', 'O4 ', 'NE2', 'NE3', 'NE4', 'NA2',
     $ 'MG2', 'AL2', 'SI2', 'SI3', 'SI4', 'S1 ', 'S2 ', 'S3 ', 'S4 '/
C
      call HI ('SEDUM')
C     !BEG
      INDEX = 0
C
      if((IONST.gt.0).and.(IONST.lt.10)) then
        write (INAME,100) QELSM(:2),IONST
  100   format(A2,I1)
        if(INAME(2:2).eq.BLANK) then
          INAME(2:3) = INAME(3:3)//BLANK
        end if
C
        call LOOKUC (TAB, NT, INAME, K, LOOK)
        if(LOOK.eq.1) then
          INDEX = K
        end if
C
        LCEX  = 1
        if(INDEX.eq.9) then
          LCEX = 3
        end if
C
      else
        call MESHED ('SEDUM', 3)
        write (LUEO,101) IONST
  101   format(' ','IONST =',I12,'; SEDUM cannot handle this --- ',
     $             'thus no lower-level charge exchange.')
        call MASHED ('SEDUM')
      end if
C     !END
      call BYE ('SEDUM')
C
      return
      end
