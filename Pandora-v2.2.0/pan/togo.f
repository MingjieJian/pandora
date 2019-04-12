      subroutine TOGO
     $(MESS,TNU,N,TNP,K,IS,IL,TITLE)
C
C     Rudolf Loeser, 1981 Jan 21
C---- Prints, for BENIN.
C     !DASH
      save
C     !DASH
      real*8 TNP, TNU
      integer IL, IQSTM, IS, K, LUEO, MESS, N
      character TEXT*20, TITLE*(*), VAL*16
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
      equivalence (IQQ(227),IQSTM)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
C     !EJECT
      external JUNO, VECOUT, LINER, HALT, HI, BYE
C
C               TNU(N), TNP(K)
      dimension TNU(*), TNP(*)
C
      dimension TEXT(3)
C
      data TEXT /'too few values      ', 'no small values     ',
     $           'max value too small '/
C
      call HI ('TOGO')
C     !BEG
      if((MESS.lt.1).or.(MESS.gt.3)) then
        write (MSSLIN(1),100) MESS
  100   format('MESS =',I12,'. which is not 1, 2, or 3.')
        call HALT   ('TOGO', 1)
      end if
C
      call JUNO
C     !EJECT
      if(IQSTM.le.0) then
        call LINER  (3, LUEO)
        write (LUEO,101) TITLE
  101   format(' ','Error in reduced-TAU procedure for WN matrix'/
     $         ' ',A100)
C
        call VECOUT (LUEO, TNU, N, 'Original TAU table, TNU')
C
        call LINER  (1, LUEO)
        write (LUEO,102) IS,IL
  102   format(' ','Selection indices',10X,2I5)
C
        call VECOUT (LUEO, TNP, K, 'Reduced TAU table, TNP')
C
        call LINER  (1, LUEO)
        if(MESS.eq.1) then
          write (LUEO,103)
  103     format(' ','Less than 4 points in reduced TAU table')
        else if(MESS.eq.2) then
          write (LUEO,104)
  104     format(' ','No small TAU values')
        else if(MESS.eq.3) then
          write (LUEO,105)
  105     format(' ','Maximum value of reduced TAU table is less ',
     $               'than 5.')
        end if
      else
        if(MESS.eq.1) then
          VAL = '                '
        else if(MESS.eq.2) then
          write (VAL,106) TNP(2)
  106     format('TNP(2)=',1PE9.2)
        else if(MESS.eq.3) then
          write (VAL,107) TNP(K)
  107     format('TNP(K)=',1PE9.2)
        end if
C
        call LINER (1, LUEO)
        write (LUEO,108) TEXT(MESS),VAL,N,K,IS,IL,TITLE
  108   format(' ','WN-matrix TAU-reduction error: ',A20,4X,A16,
     $             '  N=',I4,'  K=',I4,'  IS=',I4,'  IL=',I4,2X,
     $             '(Option STAUREDM)'/
     $         ' ',A100)
      end if
C     !END
      call BYE ('TOGO')
C
      return
      end
