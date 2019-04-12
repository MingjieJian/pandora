      subroutine SPARK
     $(N,LU,INAME,LEV,INDEX,ISO1,RKI,RLI,TE,PF,ESG,CFH,CFHE,ALL,PRNT)
C
C     Rudolf Loeser, 2004 Sep 20
C---- Prints lower-level charge exchange data.
C     (This is version 2 of SPARK.)
C     !DASH
      save
C     !DASH
      real*8 ALL, CFH, CFHE, ESG, PF, RKI, RLI, TE
      integer I, INDEX, IQLXL, LEV, LU, N
      logical ISO1, PRNT
      character BALL*15, BLANK*1, INAME*3, PALL*25
C     !COM
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
      equivalence (IQQ(333),IQLXL)
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
C     !EJECT
      external ABJECT, LINER, SHIM, HI, BYE
C
C               CFHE(N), RKI(N), RLI(N), ESG(N), CFH(N), ALL(N), TE(N),
      dimension CFHE(*), RKI(*), RLI(*), ESG(*), CFH(*), ALL(*), TE(*),
C
C               PF(N)
     $          PF(*)
C
      call HI ('SPARK')
C     !BEG
      PRNT = LU.gt.0
      if(PRNT) then
        BALL = BLANK
        if(ISO1) then
          BALL = '            ALL'
        end if
        if(LEV.eq.1) then
          call ABJECT (LU)
        else
          call LINER  (4, LU)
        end if
C
        write (LU,100) INAME,LEV,INDEX,ISO1
  100   format(' ','Lower-level charge exchange for ',A3,', level',I3,
     $             42X,'(INDEX =',I3,', ISO1 =',L3,')')
        if(IQLXL.gt.0) then
          write (LU,101)
  101     format(' ','Uses LTE hydrogen number densities ',
     $               '(option CHEXLOL = on).')
        else
          write (LU,102)
  102     format(' ','Uses actual hydrogen populations from the ',
     $               'atmosphere model (option CHEXLOL = off).')
        end if
        call LINER    (2, LU)
        write (LU,103) BALL
  103   format(' ',6X,'------ Starting values ------'/
     $         ' ',4X,'i',13X,'RK',13X,'RL',13X,'TE',13X,'PF',12X,
     $             'ESG',12X,'CFH',11X,'CFHE',A15)
        call LINER    (1, LU)
C
        PALL = BLANK
        do 106 I = 1,N
          if(ISO1) then
            write (PALL,104) ALL(I)
  104       format(1PE15.7)
          end if
          write (LU,105) I,RKI(I),RLI(I),TE(I),PF(I),ESG(I),CFH(I),
     $                     CFHE(I),PALL
  105     format(' ',I5,1P7E15.7,A15)
          call SHIM   (I, 5, LU)
  106   continue
      end if
C     !END
      call BYE ('SPARK')
C
      return
      end
