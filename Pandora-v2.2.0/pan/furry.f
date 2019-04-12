      subroutine FURRY
     $(KODE,STOP)
C
C     Rudolf Loeser, 1968 Jun 14
C     Revised RL/SGK Jul 14 2014 
C---- Reads the OPTION of DO and OMIT statements.
C     KODE=1 for "OMIT", =2 for "DO".
C     !DASH
      save
C     !DASH
      real*8 dummy
      integer IPNT, KERR, KIND, KODE, KOPT, LOOK, LUEO, MODE, jummy
      logical STOP
      character LPAREN*1, QOPTNAM*8, RPAREN*1
C     !COM
C---- OPTIONS     as of Jul 14 2014 (RL/SGK)
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=346)
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
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(49),LPAREN)
      equivalence (SYMBS(50),RPAREN)
C     !DASH
      external  MACE, KIWI, LOOKUC, MESHED, CHLOE, CARMEN, SINGC, UNMIX,
     $          OPTITOR, HI, BYE
      intrinsic max, min
C
      dimension IPNT(NOOPT)
C     !EJECT
C
      call HI ('FURRY')
C     !BEG
      KERR = 0
C---- Compute option setting
      KOPT = max(min((KODE-1),1),0)
C
      call MACE
C---- Read next field
  100 continue
      call KIWI        (MODE, dummy, jummy, QOPTNAM, jummy)
C---- Error if not alphanumeric
      if(MODE.ne.2) goto 201
      if(QOPTNAM.eq.RPAREN) then
        goto 199
      else
        call UNMIX     (QOPTNAM)
C----   Is it an option name?
        call LOOKUC    (ONAME, NOOPT, QOPTNAM, KIND, LOOK)
        if(LOOK.eq.1) then
C----     Yes - set switch and read next field
          IQQ(KIND) = KOPT
          goto 100
        else
C----     No - is it a former option?
          call OPTITOR (QOPTNAM, LOOK)
          if(LOOK.eq.1) then
C----       Yes - set signal and read next field
            STOP = .true.
            go to 100
          else
C----       No - error
            go to 202
          end if
        end if
      end if
C
C---- Error messages
  202 KERR = KERR+1
  201 KERR = KERR+1
      call SINGC       (ONAME, NOOPT, jummy, IPNT)
      call MESHED      ('FURRY', 1)
      write (LUEO,200) ONAME
  200 format(' ','Error reading OPTION names. List of valid names:'//
     $      (' ',12A10))
      call CHLOE       (LUEO, QOPTNAM, KERR)
      STOP = .true.
      if(KERR.eq.2) go to 100
      call CARMEN
C
C---- Go home
  199 continue
C     !END
      call BYE ('FURRY')
C
      return
      end
