      subroutine SHIVA
     $(EMU,L,WAVES,NW,EMINT,BRIGHT,NO,IJECT,LFB,WTAB)
C
C     Rudolf Loeser, 1980 Oct 01
C---- Controls computation of Color Temperatures.
C     (This is version 2 of SHIVA.)
C     !DASH
      save
C     !DASH
      real*8 BRIGHT, CRIT, EMINT, EMU, WAVES, WTAB
      integer IJECT, IQCOT, J, KSR, L, LFB, LU, MXKOUNT, NO, NW
      character BLANK*1, FACELAB*10
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ( 84),IQCOT)
C     !DASH
C     !EJECT
      external  ZEUS, DEJECT, LINER, ELMO, BROCAS, TUMBLE, DOUBLER,
     $          HI, BYE
C
C               EMINT(Nmkuse,L), BRIGHT(Nmkuse,L), EMU(L), WTAB(Nmkuse),
      dimension EMINT(NW,*),     BRIGHT(NW,*),     EMU(*), WTAB(*),
C
C               WAVES(Nmkuse)
     $          WAVES(*)
C
      data MXKOUNT,CRIT /20, 1.D-4/
C
      call HI ('SHIVA')
C     !BEG
      call ZEUS      (NO, IQCOT, LU)
C
      if(LU.gt.0) then
C----   Print heading
        call DEJECT  (LU, IJECT)
        call TUMBLE  (LFB, FACELAB)
        if(FACELAB(10:10).eq.BLANK) then
          write  (LU,100)
  100     format(' ',47X,'Brightness and Color Temperatures.')
        else
          write  (LU,101) FACELAB
  101     format(' ',41X,'Brightness and Color Temperatures.',2X,A10)
        end if
        call LINER   (1, NO)
        call DOUBLER (NO)
        call LINER   (2, NO)
C
C----   Initialize errors count
        KSR = 0
C
C----   Loop over all look angles
        do 102 J = 1,L
C----     Compute and print
          call ELMO  (LU, EMU(J), NW, WAVES, WTAB, EMINT(1,J),
     $                BRIGHT(1,J), MXKOUNT, CRIT, KSR)
  102   continue
C
C----   Print final error message
        call BROCAS  (LU, KSR, CRIT, MXKOUNT)
      end if
C     !END
      call BYE ('SHIVA')
C
      return
      end
