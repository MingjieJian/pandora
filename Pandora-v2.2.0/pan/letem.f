      subroutine LETEM
     $(LU,DIDG1,N,NL,MNG1,G1,GVL)
C
C     Rudolf Loeser, 1990 Apr 17
C---- Replaces GVL-1 (if needed) for the diffusion calculation.
C     !DASH
      save
C     !DASH
      real*8 G1, GVL
      integer I, IQGNV, LIMIT, LU, MNG1, N, NL
      logical DIDG1
      character BLANK*1, MARK*1, STAR*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
      equivalence (SYMBS(45),STAR  )
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
      equivalence (IQQ(244),IQGNV)
C     !DASH
      external  DILLA, DASHER, MOVE1, LINER, SHIM, HI, BYE
C
C               G1(MNG1), GVL(N,NL)
      dimension G1(*),    GVL(N,*)
C     !EJECT
C
      call HI ('LETEM')
C     !BEG
      if(DIDG1.and.(IQGNV.gt.0)) then
        call DILLA      (G1,GVL(1,1),N,MNG1,LIMIT)
C
        if(LIMIT.le.0) then
          if(LU.gt.0) then
            call LINER  (2,LU)
            call DASHER (LU)
            call LINER  (1,LU)
            write (LU,100) N,MNG1,LIMIT
  100       format(' ','None of G1 was used.',10X,'N =',I5,5X,
     $                 'MNG1 =',I5,5X,'LIMIT =',I5)
            call LINER  (1,LU)
            call DASHER (LU)
          end if
        else
          if(LU.gt.0) then
            call LINER  (2,LU)
            call DASHER (LU)
            call LINER  (1,LU)
            write (LU,101) LIMIT,MNG1
  101       format(' ','Final values of GVL-1 have been set equal to ',
     $                 'G1 up to LIMIT, because option GNVCALC = on.',
     $                 12X,'LIMIT =',I4,5X,'MNG1 =',I4//
     $             ' ','LIMIT is obtained as follows:'/
     $             ' ','When MNG1 is negative, then ',
     $                 'LIMIT is the lesser of I and -MNG1, and'/
     $             ' ',5X,'I is the smallest index such that, for all ',
     $                 'depth indices greater than I, abs[G1] is ',
     $                 'greater than or equal to abs[GVL-1].'/
     $             ' ','When MNG1 is positive, then LIMIT = MNG1.'//
     $             ' ','(NOTE: If the resulting line source functions ',
     $                 'show discontinuities at the LIMIT index, ',
     $                 'then LIMIT is too large.)')
C     !EJECT
            call LINER  (1,LU)
            write (LU,102)
  102       format(' ',14X,'old GVL-1',18X,'G1',9X,'final GVL-1')
            call LINER  (1,LU)
            do 104 I = 1,N
              MARK = BLANK
              if(I.le.LIMIT) then
                if(I.eq.LIMIT) then
                  MARK = STAR
                end if
                write (LU,103) I,MARK,GVL(I,1),G1(I),G1(I)
  103           format(' ',I4,A1,1P3E20.8)
C
                GVL(I,1) = G1(I)
C
              else
                write (LU,103) I,MARK,GVL(I,1),G1(I),GVL(I,1)
              end if
              call SHIM (I,5,LU)
  104       continue
            call LINER  (1,LU)
            call DASHER (LU)
          else
            call MOVE1  (G1,LIMIT,GVL(1,1))
          end if
        end if
      end if
C     !END
      call BYE ('LETEM')
C
      return
      end
