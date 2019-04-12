      subroutine URTICA
     $(NO,IU,IL,KODE,MODE,XI,A,K,YUSED)
C
C     Rudolf Loeser, 1989 Feb 02
C---- Prints, for GALIUM.
C     KODE = 1: symmetric
C            2: full
C            3: custom
C            4: full, augmented
C     MODE = 1: transition-specific
C          = 2: common (standard).
C     !DASH
      save
C     !DASH
      real*8 A, XI, YUSED
      integer I, IL, IQAIW, IU, K, KODE, MODE, NO
      logical LONG
      character YMSS*24
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
      equivalence (IQQ(286),IQAIW)
C     !DASH
C     !EJECT
      external LEOPOLD, CAMP, LINER, HI, BYE
C
C               XI(K), A(K)
      dimension XI(*), A(*)
C
      call HI ('URTICA')
C     !BEG
      if(NO.gt.0) then
C
        LONG = IQAIW.le.0
        if(LONG) then
          call LINER   (2, NO)
        end if
        call CAMP      (YUSED, YMSS)
C
  100   format(' ',A2,5X,1P10E12.4/(' ',7X,10E12.4))
C
        goto (101, 104, 105, 107), KODE
C
  101   continue
          if(MODE.eq.1) then
            write (NO,102) IU,IL,'XIsym',K,YMSS
  102       format(' ','Transition (',I2,'/',I2,') uses a specific ',
     $                 A,' table, of length',I6,'.',5X,A)
            if(LONG) then
              call LINER (1, NO)
              write (NO,100) 'XI', (XI(I),I=1,K)
              call LINER (1, NO)
              write (NO,100) 'A ', (A(I) ,I=1,K)
            end if
          else
            write (NO,103) IU,IL,'XIsym'
  103       format(' ','Transition (',I2,'/',I2,') uses the standard ',
     $                 A,' table.')
          end if
          goto 199
C
  104   continue
          if(MODE.eq.1) then
            write (NO,102) IU,IL,'XIfull',K,YMSS
            if(LONG) then
              call LINER (1, NO)
              write (NO,100) 'XI', (XI(I),I=1,K)
              call LINER (1, NO)
              write (NO,100) 'A ', (A(I) ,I=1,K)
            end if
          else
            write (NO,103) IU,IL,'XIfull'
          end if
          goto 199
C     !EJECT
  105   continue
          write (NO,106) IU,IL,K,YMSS
  106     format(' ','Transition (',I2,'/',I2,') uses a custom-built ',
     $               'XI table, of length',I6,'.',5X,A)
          call LINER     (1, NO)
          call LEOPOLD   (NO, IU, IL, XI, K)
          if(LONG) then
            call LINER   (1, NO)
            write (NO,100) 'XI',(XI(I),I=1,K)
            call LINER   (1, NO)
            write (NO,100) 'A ',(A(I) ,I=1,K)
          else
            call LINER   (1, NO)
          end if
          goto 199
C
  107   continue
          write (NO,108) IU,IL,K,YMSS
  108     format(' ','Transition (',I2,'/',I2,') uses an augmented ',
     $               'full XI table, of length',I6,'.',5X,A)
          if(LONG) then
            call LINER   (1, NO)
            write (NO,100) 'XI',(XI(I),I=1,K)
            call LINER   (1, NO)
            write (NO,100) 'A ',(A(I) ,I=1,K)
          else
            call LINER   (1, NO)
          end if
          goto 199
C
  199   continue
      end if
C     !END
      call BYE ('URTICA')
C
      return
      end
