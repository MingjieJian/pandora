      subroutine ELEFANT
     $(CHI,NO)
C
C     Rudolf Loeser, 1983 Mar 21
C---- Prints values of Ionization Potential.
C     (This is version 2 of ELEFANT.)
C     !DASH
      save
C     !DASH
      real*8 CHI, THSND
      integer I, IN, IQUVP, J, LUM, MUM, NO, NUM
      character BLANK*1, CHIQ*10, LINE*120
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C
C---- ELEMENT     as of 1998 Aug 17
      integer     NELX
      parameter   (NELX=50)
C     (Remember to recompile all users when changing NELX)
      real*8      ELABD,ELCHI,ELLU1,ELLU2,ELABL,ELDEF
      integer     LATNO,LDEFR,NMT,NMTMAX
      logical     LATEM
      character   ELSYM*3, ELSUB*3
      dimension   ELSYM(NELX),ELSUB(NELX),ELABD(NELX),ELCHI(NELX),
     $            ELLU1(NELX),ELLU2(NELX),ELABL(NELX),ELDEF(NELX),
     $            LATNO(NELX),LDEFR(NELX),LATEM(NELX)
C
      common      /ELEMNT0/ NMT,NMTMAX
      common      /ELEMNT1/ ELSYM,ELSUB
      common      /ELEMNT2/ ELABD,ELCHI,ELLU1,ELLU2,ELABL,ELDEF
      common      /ELEMNT3/ LATNO,LDEFR
      common      /ELEMNT4/ LATEM
C
C     Element data tables:
C             ELSYM - element symbol;
C             ELSUB - (Scratch storage for I.D. symbols);
C             ELABD - abundance (w.r.t. Hydrogen);
C             ELCHI - Chi, i.e. ionization potential;
C             ELLU1 - U-I partition function;
C             ELLU2 - U-II partition function;
C             ELABL - logarithmic abundance;
C             ELDEF - defaults values of logarithmic abundance;
C             LATNO - atomic number; and
C             LDEFR - default values sources codes.
C             LATEM - "metal" designator
C     .
C     !EJECT
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
      equivalence (IQQ(165),IQUVP)
C     !DASH
      external  LINER, JOUST, HI, BYE
      intrinsic mod
C
C               CHI(NMT)
      dimension CHI(*)
C
      dimension LUM(3)
C
      data THSND /1.D3/
C
      call HI ('ELEFANT')
C     !BEG
      if(NO.gt.0) then
        write (NO,100)
  100   format(' ','Values of Ionization Potential')
        call LINER (1, NO)
C
        call JOUST (NMT, LUM)
C     !EJECT
        J    = -40
        IN   = 1
        NUM  = 1
        MUM  = NUM
        LINE = BLANK
C
        do 104 I = 1,NMT
          if(MUM.le.NMT) then
            J = J+40
            if(CHI(MUM).ge.THSND) then
              CHIQ = '    (n.a.)'
            else
              write (CHIQ,101) CHI(MUM)
  101         format(F10.3)
            end if
            write (LINE(J+1:J+15),102) ELSYM(MUM),CHIQ
  102       format(A3,2X,A10)
            MUM = MUM+LUM(IN)
            IN  = IN+1
            if(mod(I,3).eq.0) then
              write (NO,103) LINE
  103         format(' ',A120)
              J    = -40
              IN   = 1
              NUM  = NUM+1
              MUM  = NUM
              LINE = BLANK
            end if
          end if
  104   continue
C
        if(J.ge.0) then
          write (NO,103) LINE
        end if
C
        call LINER (1, NO)
        if(IQUVP.gt.0) then
          write (NO,105)
  105     format(' ','Since option PARTVAR = on, these values were ',
     $               'obtained from the Hamburg data.')
        else
          write (NO,106)
  106     format(' ','Since option PARTVAR = off, these values were ',
     $               'obtained from the Elements table (see Section ',
     $               '10 of "About PANDORA").')
        end if
C
      end if
C     !END
      call BYE ('ELEFANT')
C
      return
      end
