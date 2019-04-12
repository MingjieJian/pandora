      program LOOKAT
C
C     Rudolf Loeser, 1993 Jul 08, 1993 Aug 16, 1993 Sep 24
C                    1994 Jul 07, 1994 Jul 26, 1994 Aug 23
C                    1994 SEP 13, 1995 Mar 13
C---- Adapted for UNIX (file names), 1995 Oct 06
C                      (carriage control), 1995 Oct 27
C                      (increase buffer), 1997 Nov 12
C                      (misc. changes), 1998 Jan 12,13
C                      (major changes), 1998 Nov-Dec
C     Under Solaris, 2001 Oct
C                    (minor change, 2002 Mar 15)
C           MXLN increased from 0.2 to 1.0 million, 2002 Aug 13
C           MXLN decreased from 1.0 to 0.4 million, 2002 Sep 05
C
C     2006 Aug 21: "complete" vs "clean" windows
C     2006 Aug 25: buffer overflow
C
C
C
C---- Displays a PANDORA output file (....aaa).
C
C
C
C     Uses an index file (....aix) if available.
C     Can produce an output file containing selected portions
C     of the input file.
C
C
      parameter (MXLN=400000, MXSC=5000, LPP=900, KPL=136, KPW=132)
C
C     !BDECL
      real*4    VERSION
      character OUTPUT*(KPL), PSN*9, PSNNAME*(KPL-10),
     $          WINDOW*(KPL), NAMEOUT*20, NAMEAAA*20, NAMEAIX*20,
     $          PRECEDE*10, COMMAND*10, STRING*(KPW), OLDSTR*(KPW),
     $          STRENG*(KPW), FF*1
      integer   LINDEX,LIND,IFF,LORENG,NLS,NCS,NGR,MORE,LININC,NOVR,
     $          INCS,ISCS,IECS,ICL,ICE,ICC,MODE,NSC,NLN,I,IDIR,
     $          KODE,KWIN,NAAA,NAAS,NAIX,NCOL,NL,NWIN,KF,KL,LF,LL,
     $          KFS,KLS,LFS,LLS,NSI,NSS,NSE,MXLN,MXSC,LPP,KPL,KPW,
     $          KOMM,NOFF,KVN,JUNK,NLC,KVS,KCH
      logical   STOP, lummy, BAD, FULL
C     !EDECL
C     !COM
      common /carriage/ FF
      common /control/ NGR,NLS,NCS,INCS,ISCS,IECS,ICL,ICE,ICC,
     $                 MODE,MORE,NSC,NLN,LININC,NOVR
      common /current/ KF,KL,LF,LL,KVN, KFS,KLS,LFS,LLS,NSI,NSS,NSE,KVS
C     !DASH
C
      dimension OUTPUT(MXLN,2), PSN(MXSC), PSNNAME(MXSC), WINDOW(LPP),
     $          LINDEX(MXSC,2), LIND(LPP), KFS(9), KLS(9), JUNK(MXSC),
     $          LFS(9), LLS(9), NSI(9), NSS(9), NSE(9), KVS(9)
C     !EJECT
      external  CLEAR_WINDOW, INIT_OUT, SECT_OUT, SIGN_ON, TYPE_WINDOW,
     $          FILL_WINDOW, INTERPRETE, SET_SECTION, TYPE_INDEX,
     $          WINDOW_OUT, FIND_STRING, READ_AAA, SET_WINDOW,
     $          TYPE_INFO, GET_COMMAND, READ_AIX, SIGN_OFF,
     $          TYPE_MENU_1, TYPE_MENU_2
      intrinsic char
C
      data PSN     /MXSC*' '/
      data PSNNAME /MXSC*' '/
      data (LINDEX(I,1),I=1,MXSC) /MXSC*0/
      data (LINDEX(I,2),I=1,MXSC) /MXSC*0/
C
      data KF,KL,LF,LL /1,KPW, 1,LPP/
      data (KFS(I),I=1,9) /9*0/
      data (KLS(I),I=1,9) /9*0/
      data (LFS(I),I=1,9) /9*0/
      data (LLS(I),I=1,9) /9*0/
      data (NSI(I),I=1,9) /9*0/
      data (NSS(I),I=1,9) /9*0/
      data (NSE(I),I=1,9) /9*0/
      data (KVS(I),I=1,9) /9*0/
C
      data IFF /'0C'X/
      data STRENG, LORENG /' ', 0/
      data NLS, NCS, NGR, MORE /60, 132, 10, 0/
      data LININC, NOVR /59, 0/
      data INCS /-1/
C
      data VERSION /4.20/
C     !EJECT
C     Control parameter definitions:
C----
C     NGR   = the number of items in a "group" of items (default = 10)
C----
C     NLS   = number of lines in the "virtual" window (default = 58)
C     NCS   = number of characters per line of the
C             "virtual" window (default = 132)
C----
C     INCS  = "section" index, i.e. an index such that
C             OUTPUT[LINDEX(INCS)] is the first line of the
C             current printout section (a "page" or a "PSN section")
C----
C     ISCS  = section "start line" index, i.e. an index such that
C             OUTPUT(ISCS) is the first line of the current section
C                    ISCS = LINDEX(INCS)
C     IECS  = section "end line" index, i.e. an index such that
C             OUTPUT(IECS) is the last line of the current section
C                    IECS = LINDEX(INCS+1)-1 when INCS .lt. NSC
C                    IECS = NLN when INCS .eq. NSC
C----
C     ICL   = "virtual window line" index, i.e. an index such
C             that OUTPUT(ICL) is the top-most line of the current
C             "virtual" window
C
C     ICE   = "virtual window end" index, i.e. an index such that
C             OUTPUT(ICE) is the bottom-most line of the current
C             "virtual" window
C             
C     ICC   = "virtual window character" index, i.e. an index such
C             that the character OUTPUT(j)(ICC:ICC) of each line j
C             in the current section is the left-most character of
C             the current "virtual" window
C----
C     MODE  = 1 means: "sections" begin with PSN markers
C     MODE  = 2 means: "sections" begin with form-feeds
C
C     MORE  = 0 means: display mode is "safe"
C     MORE  = 1 means: display mode is "sorry"
C
C     NSC   = actual number of sections
C     NLN   = actual number of lines, "complete"
C     NLC   = actual number of lines, "clean"
C
C---- Current window:
C             LF = first line,              LL = last line
C             KF = pos. of first character, KL = pos. of last character
C     !EJECT
C     !beg
      FF = char(IFF)
C
   98 continue
C---- Sign on, set up files, and establish MODE
      call SIGN_ON       (NAAA,NAMEAAA,NAIX,NAMEAIX,NAAS,NAMEOUT,
     $                    VERSION,MODE,STOP)
      if(STOP) goto 999
C---- Type general information and accept modified control parameters
      call TYPE_INFO     (MODE,LPP,KPW,NLS,NCS,NGR)
      if(MODE.eq.1) then
C----   Read index file
        call READ_AIX    (NAIX,NAMEAIX,PSN,PSNNAME,MXSC,NSC)
      end if
C
      NLN = 0
      NLC = 0
      KCH = 1
      rewind NAAA
   99 continue
C---- Read (all, or part of) PANDORA output file
      call READ_AAA      (NAAA,NAMEAAA,OUTPUT,MXLN,NLN,NLC,BAD,MODE,
     $                    LINDEX,NSC,MXSC,JUNK,KPL,KCH,FULL)
C---- Compose initial window 
      call CLEAR_WINDOW  (WINDOW,LIND,NLS)
      KVN  = 1
      call SET_SECTION   (LINDEX(1,KVN),1,.false.,lummy)
      ICL  = ISCS
      ICE  = 0
      ICC  = 1
      call SET_WINDOW    (KPL,0)
      call FILL_WINDOW   (OUTPUT(1,KVN),WINDOW,LIND,NL)
  100 continue
C----   Get next user command
        call GET_COMMAND (PSN,PRECEDE,COMMAND)
C----   Digest this command
        call INTERPRETE  (PRECEDE,COMMAND,OUTPUT(1,KVN),LINDEX(1,KVN),
     $                    NCOL,PSN,LPP,KPL,KPW,IDIR,OLDSTR,STRING,
     $                    NWIN,NAMEAAA,KOMM,NOFF,STOP,KODE)
        if(STOP) goto 999
        goto (101,102,103,104,105,106,107,108,109,110,111,112,
     $        999), KODE
C
  101   continue
C----     Display the current window unchanged
          call TYPE_WINDOW (WINDOW,LIND,NL)
          goto 100
C
  102   continue
C----     Type main command menu
          if(KOMM.eq.1) then
            call TYPE_MENU_1 (VERSION,NOFF)
          else if(KOMM.eq.2) then
            call TYPE_MENU_2 (VERSION,NOFF)
          end if
          goto 100
C     !EJECT
  103   continue
C----     Type index
          call TYPE_INDEX  (PSN,PSNNAME,KVN,LINDEX(1,KVN),JUNK,NCOL)
          goto 100
C
  104   continue
C----     Modify and display window
          call SET_WINDOW  (KPL,0)
          call FILL_WINDOW (OUTPUT(1,KVN),WINDOW,LIND,NL)
          call TYPE_WINDOW (WINDOW,LIND,NL)
          goto 100
C
  105   continue
C----     Copy current section to output file
          call INIT_OUT    (NAAS,NAMEOUT,OUTPUT(1,KVN),KPL)
          call SECT_OUT    (NAAS,OUTPUT(1,KVN),KPL,ISCS,IECS)
          goto 100
C
  106   continue
C----     Copy current window to output file
          call INIT_OUT    (NAAS,NAMEOUT,OUTPUT(1,KVN),KPL)
          call WINDOW_OUT  (NAAS,WINDOW,LIND,NL,INCS,PSN,MODE)
          goto 100
C
  107   continue
C----     Nothing
          goto 100
C
  108   continue
C----     Change to new Pandora files
          call SIGN_OFF    (NAAA,NAIX,NAAS,NAMEOUT,VERSION)
          goto 98
C
  109   continue
C----     Find specified string
          call FIND_STRING (STRING,IDIR,OUTPUT(1,KVN),LINDEX(1,KVN),
     $                      STRENG,LORENG)
          goto 104
C     !EJECT
  110   continue
C----     Display or jump to "named" window
          KWIN = NWIN
          call SET_WINDOW  (0,KWIN)
          if(KWIN.lt.0) goto 107
          call FILL_WINDOW (OUTPUT(1,KVN),WINDOW,LIND,NL)
          call TYPE_WINDOW (WINDOW,LIND,NL)
          goto 100
C
  111   continue
C----     Change output filter mode
          if(KVN.eq.2) then
            KVN = 1
          else
            if(BAD) then
              print 200
  200         format(' ','"Clean" mode is not available due to ',
     $                   'problem(s) with the output file.')
              goto 101
            else
              KVN = 2
            end if
          end if
          call SET_SECTION (LINDEX(1,KVN),INCS,.false.,lummy)
          ICL = ISCS
          ICE = 0
          goto 104
C
  112   continue
C----     Read the next part of the PANDORA output file
          if(FULL) then
            goto 99
          else
            print 201
  201       format(' ','Nothing remains to be read.')
            goto 107
          end if
C
  999   continue
C----     Exit
          call SIGN_OFF    (NAAA,NAIX,NAAS,NAMEOUT,VERSION)
          stop 'LOOKAT: done'
C     !end
      end
      subroutine CLEAR_WINDOW
     $(WINDOW,LIND,N)
C
C     Rudolf Loeser, 1993 Jul 13
C
C---- Clears the WINDOW buffer.
C     !DASH
      save
C     !BDECL
      integer   LIND,I,N
      character WINDOW*(*)
C     !EDECL
C     !DASH
      dimension WINDOW(*), LIND(*)
C     !beg
      do 100 I = 1,N
        WINDOW(I) = ' '
        LIND(I)   = 1
  100 continue
C     !end
      return
      end
      subroutine DECODE_COMMAND
     $(COMMAND, LENGTH,INDEX,NUMBER)
C
C     Rudolf Loeser, 1994 Aug 29
C
C---- Decodes the current command; returns
C       LENGTH - length of the command, in characters
C       INDEX  - command code
C       NUMBER - associated numerical value (if any)
C
      parameter (NCOMM=47)
C     !DASH
      save
C     !BDECL
      integer   INDEX,LENGTH,NUMBER,I,L,KPOS,MULT,NCOMM
      character COMMAND*10, KOMMAND*10, CODES*3, DGT*1
C     !EDECL
C     !DASH
      dimension CODES(NCOMM)
C
      data CODES /
     $ 'PSN', 'pag', 'tl ', 't  ', 'ul ', 'ug ', 'm  ', 'dg ',
C       1      2      3      4      5      6      7      8
C
     $ 'dl ', 'b  ', 'le ', 'lg ', 'l  ', 'r  ', 'rg ', 're ',
C       9      10     11     12     13     14     15     16
C
     $ 'cg ', 'ch ', 'cw ', 'ps ', 'pw ', 'h  ', 'i  ', 'rw ',
C       17     18     19     20     21     22     23     24
C
     $ 'e  ', 'q  ', 'bl ', 'n  ', 'p  ', 'tv ', 'tp ', 'u  ',
C       25     26     27     28     29     30     31     32
C
     $ 'd  ', 'k  ', 'cf ', 'cm ', 'sf ', 'sb ', 'wd ', 'w  ',
C       33     34     35     36     37     38     39     40
C
     $ 'df ', 'uf ', 'j  ', 'fn ', 'a  ', 'cx ', '+  '/
C       41     42     43     44     45     46     47
C
C     !beg
      KOMMAND = COMMAND
      INDEX   = 0
      NUMBER  = 0
C---- Determine the length of the command
      LENGTH = 10
  100 continue
        if(COMMAND(LENGTH:LENGTH).eq.' ') then
          LENGTH = LENGTH-1
          if(LENGTH.eq.0) goto 104
          goto 100
        end if
C     !EJECT
C---- Recover numeric part (if any) and replace with blanks
      MULT = 1
      KPOS = LENGTH+1
  101 continue
        KPOS = KPOS-1
        if(KPOS.gt.1) then
          DGT = KOMMAND(KPOS:KPOS)
          if((DGT.eq.'0').or.(DGT.eq.'1').or.(DGT.eq.'2').or.
     $       (DGT.eq.'3').or.(DGT.eq.'4').or.(DGT.eq.'5').or.
     $       (DGT.eq.'6').or.(DGT.eq.'7').or.(DGT.eq.'8').or.
     $       (DGT.eq.'9')) then
            read (DGT,102) L
  102       format(I1)
            NUMBER = NUMBER+MULT*L
            MULT   = MULT*10
            KOMMAND(KPOS:KPOS) = ' '
            goto 101
          end if
        end if
C---- Identify command
      do 103 I=1,NCOMM
        if(KOMMAND(:3).eq.CODES(I)) then
          INDEX = I
          goto 104
        end if
  103 continue
C
  104 continue
C     !end
      return
      end
      subroutine FILL_WINDOW
     $(OUTPUT,WINDOW,LIND,N)
C
C     Rudolf Loeser, 1993 Jul 16
C
C---- Fills the WINDOW buffer.
C     !DASH
      save
C     !BDECL
      integer   LIND,N,J,L,KF,KL,LF,LL,KFS,KLS,LFS,LLS,NGR,NLS,NCS,
     $          INCS,ISCS,IECS,ICL,ICE,ICC,MODE,MORE,NSC,NLN,LININC,
     $          NOVR,NSI,NSS,NSE,KVS,KVN
      character OUTPUT*(*),WINDOW*(*)
C     !EDECL
C     !COM
      common /current/ KF,KL,LF,LL,KVN, KFS,KLS,LFS,LLS,NSI,NSS,NSE,KVS
      common /control/ NGR,NLS,NCS,INCS,ISCS,IECS,ICL,ICE,ICC,
     $                 MODE,MORE,NSC,NLN,LININC,NOVR
C     !DASH
      external LINE_LENGTH
C
      dimension OUTPUT(*), WINDOW(*), LIND(*)
      dimension KFS(9), KLS(9), LFS(9), LLS(9), NSI(9), NSS(9), NSE(9),
     $          KVS(9)
C     !beg
      N  = 0
      do 103 L = LF,LL
        N         = N+1
        WINDOW(N) = ' '//OUTPUT(L)(KF:KL)
        if((MODE.eq.1).and.(OUTPUT(L)(1:1).eq.'1')) then
C
          do 100 J = 2,NCS
            if(WINDOW(N)(J:J).eq.' ') then
              WINDOW(N)(J:J) = '_'
            end if
  100     continue
          LIND(N) = NCS
C
        else
          call LINE_LENGTH (WINDOW(N),(KL-KF+1),LIND(N))
        end if
  103 continue
C     !end
      return
      end
      subroutine FIND_FF
     $(OUTPUT, ISTART,IEND,INC, INDEX,FOUND)
C
C     Rudolf Loeser, 1998 Dec 02
C
C---- Looks for a line with a form-feed.
C     !DASH
      save
C     !BDECL
      integer   ISTART,IEND,INC,INDEX,I,IDIFF
      character OUTPUT*(*)
      logical   FOUND
C     !EDECL
C     !DASH
      external  IS_TOP_OF_PAGE
      intrinsic isign
C
      dimension OUTPUT(*)
C     !beg
      INDEX = IEND
      IDIFF = IEND-ISTART
      FOUND = (IDIFF.ne.0).and.(isign(1,IDIFF).eq.isign(1,INC))
      if(FOUND) then
        do 100 I = ISTART,IEND,INC
          INDEX = I
          call IS_TOP_OF_PAGE (OUTPUT(INDEX),FOUND)
          if(FOUND) goto 101
  100   continue
      end if
  101 continue
C     !end
      return
      end
      subroutine FIND_STRING
     $(STRING,IDIR,OUTPUT,LINDEX,STRENG,LORENG)
C
C     Rudolf Loeser, 1994 Sep 07
C
C---- Searches for STRING, and resets ICL so that STRING appears
C     near the middle of the window.
C     !DASH
      save
C     !BDECL
      integer   LINDEX,IDIR,LORENG,JP,JE,ISTART,J,I,INDX
      integer   KF,KL,LF,LL,KFS,KLS,LFS,LLS,NGR,NLS,NCS,INCS,ISCS,
     $          IECS,ICL,ICE,ICC,MODE,MORE,NSC,NLN,LININC,NOVR,
     $          NSI,NSS,NSE,KVS,KVN
      character STRING*(*), STRENG*(*), OUTPUT*(*), DUMMY*1
      logical   SAMSTR, INWNDW, lummy
C     !EDECL
C     !COM
      common /current/ KF,KL,LF,LL,KVN, KFS,KLS,LFS,LLS,NSI,NSS,NSE,KVS
      common /control/ NGR,NLS,NCS,INCS,ISCS,IECS,ICL,ICE,ICC,
     $                 MODE,MORE,NSC,NLN,LININC,NOVR
C     !DASH
      external  SEARCH_BACKWARD, SEARCH_FORWARD, SET_SECTION
      intrinsic index, max
C
      dimension LINDEX(*), OUTPUT(*)
      dimension KFS(9), KLS(9), LFS(9), LLS(9), NSI(9), NSS(9), NSE(9),
     $          KVS(9)
C     !beg
      JP = index(STRING(2:),'"')
      JE = index(STRENG(2:),'"')
      SAMSTR = STRING(2:JP).eq.STRENG(2:JE)
      INWNDW = (LORENG.ge.LF).and.(LORENG.le.LL)
      if(SAMSTR.and.INWNDW) then
        ISTART = LORENG
      else
        ISTART = ICL
      end if
      J = 0
C
      if(IDIR.gt.0) then
C----   Search forward
        ISTART = ISTART+1
        if(ISTART.gt.NLN) then
          goto 100
        else
          call SEARCH_FORWARD  (OUTPUT,ISTART,NLN,STRING(2:JP),J)
        end if
      else
C----   Search backward
        ISTART = ISTART-1
        if(ISTART.lt.1) then
          goto 100
        else
          call SEARCH_BACKWARD (OUTPUT,ISTART,STRING(2:JP),J)
        end if
      end if
C     !EJECT
  100 continue
      if(J.eq.0) then
        print 101
  101   format(' ','End of output file: target string not found. ',
     $             'Please press <Return>.',$)
        accept 102, DUMMY
  102   format(A)
        goto 106
      end if
C
C---- Identify section
      do 103 I=2,NSC
        if((J.ge.LINDEX(I-1)).and.(J.lt.LINDEX(I))) then
          INDX = I-1
          goto 105
        end if
  103 continue
      if(J.le.NLN) then
        INDX = NSC
        goto 105
      end if
C
      print 104
  104 format(' ','Sorry, LOOKAT is confused; please press <Return> ',
     $           'and then try something else.',$)
      accept 102, DUMMY
      goto 106
C
  105 continue
C---- Remember result of current search
      STRENG = STRING
      LORENG = J
C---- Set up control indices
      call SET_SECTION (LINDEX,INDX,.true.,lummy)
      ICL = max(ISCS,(J-(NLS/2)))
      ICE = 0
C
  106 continue
C     !end
      return
      end
      subroutine FIX_BOTTOM
     $(OUTPUT,ITOP,IBOT)
C
C     Rudolf Loeser, 1998 Dec 02
C
C---- Refines IBOT, the trial value of the bottom-line-index for
C     the window whose first-line-index is ITOP.
C     !DASH
      save
C     !BDECL
      integer   ITOP,IBOT,INDEX
      character OUTPUT*(*)
      logical   FOUND
C     !EDECL
C     !DASH
      external FIND_FF
C
      dimension OUTPUT(*)
C     !beg
      call FIND_FF (OUTPUT, (ITOP+1),IBOT,+1, INDEX,FOUND)
      if(FOUND) then
        IBOT = INDEX-1
      end if
C     !end
      return
      end
      subroutine GET_COMMAND
     $(PSN,PRECEDE,COMMAND)
C
C     Rudolf Loeser, 1993 Jul 20
C
C---- Gets the next command from the user
C     !DASH
      save
C     !BDECL
      integer   NGR,NLS,NCS,INCS,ISCS,IECS,ICL,ICE,ICC,MODE,MORE,
     $          NSC,NLN,LININC,NOVR
      character PSN*(*),PRECEDE*(*),COMMAND*(*),SECTION*9
C     !EDECL
C     !COM
      common /control/ NGR,NLS,NCS,INCS,ISCS,IECS,ICL,ICE,ICC,
     $                 MODE,MORE,NSC,NLN,LININC,NOVR
C     !DASH
      dimension PSN(*)
C     !beg
      if(MORE.eq.0) then
        if(MODE.eq.1) then
          SECTION = PSN(INCS)
        else
          if(INCS.gt.9999) then
            SECTION = 'PAGEnnnnn'
          else if(INCS.gt.999) then
            write (SECTION,100) INCS
  100       format('PAGE',I4)
          else if(INCS.gt.99) then
            write (SECTION,101) INCS
  101       format('PAGE',I3)
          else if(INCS.gt.9) then
            write (SECTION,102) INCS
  102       format('PAGE',I2)
          else
            write (SECTION,103) INCS
  103       format('PAGE',I1)
          end if
        end if
C
        print 104, SECTION
  104   format(' ','Section ',A,'.  Please type next command, ',
     $             'followed by <Return> (h for help): ',$)
      end if
      PRECEDE = COMMAND
      COMMAND = ' '
      accept 105, COMMAND
  105 format(A)
C     !end
      return
      end
      subroutine INIT_OUT
     $(NAAS,NAMEOUT,OUTPUT,KPL)
C
C     Rudolf Loeser, 1993 Jul 13
C
C---- Initializes the hardcopy output file.
C     !DASH
      save
C     !BDECL
      integer   NAAS,KPL,IE,I,J,K
      character OUTPUT*(*),NAMEOUT*(*)
C     !EDECL
C     !DASH
      external  LINE_LENGTH
      intrinsic index
C
      dimension OUTPUT(*)
C     !beg
      if(NAAS.le.0) then
        NAAS = 12
        open (unit=NAAS, file=NAMEOUT, status='NEW', form='FORMATTED')
        rewind NAAS
C
        IE = 0
        do 100 I = 1,150
          J = index(OUTPUT(I),'P A N D O R A')
          if(J.gt.0) then
            IE = I
            goto 101
          end if
  100   continue
  101   continue
C
        do 103 I = 1,10
          write (NAAS,102)
  102     format(' ')
  103   continue
C
        if(IE.gt.0) then
          do 105 I = IE,(IE+20)
            call LINE_LENGTH (OUTPUT(I),KPL,K)
            write (NAAS,104) OUTPUT(I)(:K)
  104       format(A)
  105     continue
        else
          write (NAAS,106)
  106     format(' ','Unidentified Pandora output file')
        end if
C
      end if
C     !end
      return
      end
      subroutine INTERPRETE
     $(PRECEDE,COMMAND,OUTPUT,LINDEX,NCOL,PSN,LPP,KPL,KPW,IDIR,OLDSTR,
     $ STRING,NWIN,NAMEAAA,KOMM,NOFF,STOP,KODE)
C
C     Rudolf Loeser, 1993 Jul 21
C
C---- Preliminary processing of commands; this includes determination
C     of all virtual window control parameters.
C     !DASH
      save
C     !BDECL
      integer   LINDEX,NGR,NLS,NCS,INCS,ISCS,IECS,ICL,ICE,ICC,MODE,
     $          MORE,NSC,NLN,LININC,NOVR,NCOL,LPP,KPL,KPW,IDIR,
     $          NWIN,KODE,I,IM,JM,JP,KC,KIND,KM,LD,LM,MC,MULT,NLSE,
     $          NUMB,KOUNT,KOMM,NOFF
      character PSN*(*),STRING*(*),OLDSTR*(*),OUTPUT*(*)
      character PRECEDE*10,COMMAND*10,NAMEAAA*20
      logical   STOP,CHANGE,OK,lummy
C     !EDECL
C     !COM
      common /control/ NGR,NLS,NCS,INCS,ISCS,IECS,ICL,ICE,ICC,
     $                 MODE,MORE,NSC,NLN,LININC,NOVR
C     !DASH
      external  DECODE_COMMAND, MOVE_TO_TOP, SET_SECTION, MOVE_DOWN,
     $          MOVE_UP, TYPE_PATTERN, MOVE_DOWN_TO_FF, SAVE_WINDOW,
     $          MOVE_UP_TO_FF, MOVE_TO_BOTTOM
      intrinsic max, min, index
C
      dimension PSN(*), OUTPUT(*), LINDEX(*)
C     !beg
      STOP = .false.
C---- Set "effective window length" and "overlap control"
      if(MORE.eq.0) then
        NLSE = NLS
        NOVR = 3
        NOFF = 2
      else
        NLSE = NLS+1
        NOVR = 0
        NOFF = 0
      end if
      LININC = NLSE-1
C     !EJECT
C---- Decode the command: MC   = length of command (characters)
C                         KIND = command code
C                         NUMB = associated input value
C
      call DECODE_COMMAND (COMMAND, MC,KIND,NUMB)    
      if(MC.eq.0) then
        if(MORE.eq.0) then
          if((PRECEDE(1:2).eq.'sf').and.(STRING(1:1).eq.'"')) then
            IDIR    = +1
            COMMAND = PRECEDE
            KODE    = 9
            goto 999
          else if((PRECEDE(1:2).eq.'sb').and.(STRING(1:1).eq.'"')) then
            IDIR    = -1
            COMMAND = PRECEDE
            KODE    = 9
            goto 999
          else
            KODE = 7
            goto 999
          end if
        else
          KIND = 33
          goto 102
        end if
      else
        if(KIND.eq.0) then
          KODE = 7
          goto 999
        end if
      end if
  102 continue
      goto (1001, 1002, 1003, 1004, 1005, 1006, 1007, 1008, 1009, 1010,
     $      1011, 1012, 1013, 1014, 1015, 1016, 1017, 1018, 1019, 1020,
     $      1021, 1022, 1023, 1024, 1025, 1026, 1027, 1028, 1029, 1030,
     $      1031, 1032, 1033, 1034, 1035, 1036, 1037, 1038, 1039, 1040,
     $      1041, 1042, 1043, 1044, 1045, 1046, 1047) KIND
C     !EJECT
 1001 continue
        if(MODE.eq.2) then
          print 103
  103     format(' ','When no index (file .aix) is used, then the ',
     $               '"PSN" command is unacceptable; use "page".')
          KODE = 7
          goto 999
        end if
C----   Set PSN-section
        do 104 I = 1,NSC
          if(PSN(I).eq.COMMAND(:MC)) then
            call SET_SECTION (LINDEX,I,.true.,OK)
            call MOVE_TO_TOP (OUTPUT,lummy)
            KODE = 4
            goto 999
          end if
  104   continue
        print 105, COMMAND
  105   format(' ','Unrecognizable PSN-section specification: ',A)
        KODE = 7
        goto 999
 1002 continue
        if(MODE.eq.1) then
          print 106
  106     format(' ','When the index (file .aix) is used, then the ',
     $               '"page" command is unacceptable; ',
     $               'use "PSN" or "k".')
          KODE = 7
          goto 999
        end if
C----   Set page-section
        call SET_SECTION (LINDEX,NUMB,.true.,OK)
        if(OK) then
          call MOVE_TO_TOP (OUTPUT,lummy)
          KODE = 4
        else
          KODE = 7
        end if
        goto 999
C     !EJECT
 1003 continue
C----   Move window to top-left
        if((ICL.ne.ISCS).or.(ICC.ne.1)) then
          ICC = 2
          call MOVE_TO_TOP (OUTPUT,lummy)
          KODE = 4
        else
          KODE = 7
        end if
        goto 999
 1004 continue
C----   Move window to top
        call MOVE_TO_TOP (OUTPUT,CHANGE)
        if(CHANGE) then
          KODE = 4
        else
          KODE = 7
        end if
        goto 999
 1005 continue
C----   Move window up 1 line
        if(ICL.gt.ISCS) then
          ICL  = ICL-1
          ICE  = 0
          KODE = 4
        else
          KODE = 7
        end if
        goto 999
 1006 continue
C----   Move window up 1 group
        call MOVE_UP (OUTPUT,NGR,0,CHANGE)
        if(CHANGE) then
          KODE = 4
        else
          KODE = 7
        end if
        goto 999
 1007 continue
C----   Move window to middle
        LM = ((ISCS+IECS)-NLSE)/2
        IM = max(LM,ISCS)
        if(ICL.ne.IM) then
          ICL  = IM
          ICE  = 0
          KODE = 4
        else
          KODE = 7
        end if
        goto 999
C     !EJECT
 1008 continue
C----   Move window down 1 group
        call MOVE_DOWN (OUTPUT,NGR,0,CHANGE)
        if(CHANGE) then
          KODE = 4
        else
          KODE = 7
        end if
        goto 999
 1009 continue
C----   Move window down 1 line
        LD = ICL+1
        JM = max(((IECS+1)-NLSE),ISCS)
        IM = min(LD,JM)
        if(ICL.ne.IM) then
          ICL  = IM
          ICE  = 0
          KODE = 4
        else
          KODE = 7
        end if
        goto 999
 1010 continue
C----   Move window to bottom
        call MOVE_TO_BOTTOM (OUTPUT,CHANGE)
        if(CHANGE) then
          KODE = 4
        else
          KODE = 7
        end if
        goto 999
 1011 continue
C----   Move window to left edge
        if(ICC.ne.2) then
          ICC  = 2
          KODE = 4
        else
          KODE = 7
        end if
        goto 999
 1012 continue
C----   Move window left 1 group
        if(ICC.ne.2) then
          ICC  = max((ICC-NGR),2)
          KODE = 4
        else
          KODE = 7
        end if
        goto 999
C     !EJECT
 1013 continue
C----   Move window left 1 character
        if(ICC.gt.2) then
          ICC  = ICC-1
          KODE = 4
        else
          KODE = 7
        end if
        goto 999
 1014 continue
C----   Move window right 1 character
        KM = min((ICC+1),(KPL+1-NCS))
        if(ICC.ne.KM) then
          ICC  = KM
          KODE = 4
        else
          KODE = 7
        end if
        goto 999
 1015 continue
C----   Move window right 1 group
        KM = min((ICC+NGR),(KPL+1-NCS))
        if(ICC.ne.KM) then
          ICC  = KM
          KODE = 4
        else
          KODE = 7
        end if
        goto 999
 1016 continue
C----   Move window to right edge
        KM = max((KPL+1-NCS),2)
        if(ICC.ne.KM) then
          ICC  = KM
          KODE = 4
        else
          KODE = 7
        end if
        goto 999
C     !EJECT
 1017 continue
C----   Change group size
        if(NUMB.gt.0) then
          NGR = NUMB
        else
          print 109, COMMAND
  109     format(' ','The command as received is ',A,'; a value ',
     $               'greater than zero is required.')
        end if
        KODE = 7
        goto 999
 1018 continue
C----   Change window height
        if(NUMB.gt.0) then
          NLS = min(NUMB,LPP)
          if(MORE.eq.0) then
            NLSE = NLS
          else
            NLSE = NLS+1
          end if
          KODE = 4
        else
          print 109, COMMAND
          KODE = 7
        end if
        goto 999
 1019 continue
C----   Change window width
        if(NUMB.gt.0) then
          NCS  = min(NUMB,KPW)
          KODE = 4
        else
          print 109, COMMAND
          KODE = 7
        end if
        goto 999
C     !EJECT
 1020 continue
C----   Copy current section to output file
        KODE = 5
        goto 999
 1021 continue
C----   Copy current window to output file
        KODE = 6
        goto 999
 1022 continue
C----   Type main menu
        KOMM = 1
        KODE = 2
        goto 999
 1023 continue
C----   Type index
        NCOL = NUMB
        if(MODE.eq.1) then
          KODE = 3
        else
          print 223
  223     format(' ','No ".aix" file was provided; ',
     $               'therefore, there is no index.')
          KODE = 7
        end if
        goto 999
 1024 continue
C----   Restore current window on screen
        KODE = 1
        goto 999
 1025 continue
C----   Exit
        KODE = 13
        goto 999
 1026 continue
C----   Quit
        KODE = 13
        goto 999
 1027 continue
C----   Move window to bottom-left
        if((ICE.ne.IECS).or.(ICC.ne.2)) then
          ICC = 2
          call MOVE_TO_BOTTOM (OUTPUT,lummy)
          KODE = 4
        else
          KODE = 7
        end if
        goto 999
C     !EJECT
 1028 continue
C----   Advance to succeeding section
        call SET_SECTION   (LINDEX,(INCS+1),.true.,OK)
        if(OK) then
          call MOVE_TO_TOP (OUTPUT,lummy)
          KODE = 4
        else
          KODE = 7
        end if
        goto 999
 1029 continue
C----   Retreat to preceding section
        call SET_SECTION   (LINDEX,(INCS-1),.true.,OK)
        if(OK) then
          call MOVE_TO_TOP (OUTPUT,lummy)
          KODE = 4
        else
          KODE = 7
        end if
        goto 999
 1030 continue
C----   Type current parameter values
        print 110, NGR,NLS,NCS
  110   format(' ','Current values of control parameters:'/
     $         ' ',5X,'g =',I5,' group size'/
     $         ' ',5X,'h =',I5,' window height (lines)'/
     $         ' ',5X,'w =',I5,' window width (characters)')
        KODE = 7
        goto 999
 1031 continue
C----   Type test pattern
        call TYPE_PATTERN (LPP)
        KODE = 7
        goto 999
 1032 continue
C----   Move window up NUMB windows
        MULT = max(NUMB,1)
        call MOVE_UP   (OUTPUT,(MULT*NLSE),1,CHANGE)
        if(CHANGE) then
          KODE = 4
        else
          KODE = 7
        end if
        goto 999
 1033 continue
C----   Move window down NUMB windows
        MULT = max(NUMB,1)
        call MOVE_DOWN (OUTPUT,(MULT*NLSE),1,CHANGE)
        if(CHANGE) then
          KODE = 4
        else
          KODE = 7
        end if
        goto 999
C     !EJECT
 1034 continue
C----   Abbreviated PSN command: K
        if(MODE.eq.2) then
          print 111
  111     format(' ','When no index (file .aix) is used the ',
     $               '"k" command is unacceptable; use "page".')
          KODE = 7
          goto 999
        end if
C       Set PSN-section
        KC = 11-MC
        do 112 I = 1,NSC
          if(PSN(I)(KC:).eq.COMMAND(2:)) then
            call SET_SECTION (LINDEX,I,.true.,OK)
            call MOVE_TO_TOP (OUTPUT,lummy)
            KODE = 4
            goto 999
          end if
  112   continue
        print 113, COMMAND
  113   format(' ','Unrecognizable PSN-section abbreviation: ',A)
        KODE = 7
        goto 999
 1035 continue
C----   Change files command
        KODE = 8
        goto 999
 1036 continue
C----   Change display mode
        if(MORE.eq.0) then
          MORE = 1
          NLSE = NLS+1
          NOVR = 0
        else
          MORE = 0
          NLSE = NLS
          NOVR = 3
        end if
        KODE = 4
        goto 999
C     !EJECT
 1037 continue
        IDIR = +1
        goto 114
 1038 continue
        IDIR = -1
C----   Search command
  114   continue
        KOUNT = 0
  115   continue
          KOUNT = KOUNT+1
          print 116
  116     format(' ','Type search target, in "double quotes", ',
     $               'then <Return>: ',$)
          OLDSTR = STRING
          STRING = ' '
          accept 117, STRING
  117     format(A)
          if((STRING(1:1).eq.' ').and.(OLDSTR(1:1).eq.'"')) then
            STRING = OLDSTR
            KODE   = 9
            goto 999
          else if(STRING(1:1).eq.'"') then
            JP = index(STRING(2:),'"')
            if(JP.le.0) goto 118
            KODE = 9
            goto 999
          end if
  118     continue
          if(KOUNT.lt.3) then
            goto 115
          else
            STOP = .true.
            goto 999
          end if
C     !EJECT
 1039 continue
C----   Name a window
        call SAVE_WINDOW (NUMB)
        KODE = 7
        goto 999
 1040 continue
C----   Display the named window
        NWIN = NUMB
        KODE = 10
        goto 999
 1041 continue
C----   Move down to next ff
        call MOVE_DOWN_TO_FF (OUTPUT,CHANGE)
        if(CHANGE) then
          KODE = 4
        else
          KODE = 7
        end if
        goto 999
 1042 continue
C----   Move up to next ff
        call MOVE_UP_TO_FF (OUTPUT,CHANGE)
        if(CHANGE) then
          KODE = 4
        else
          KODE = 7
        end if
        goto 999
 1043 continue
C----   Jump to the named window
        NWIN = -NUMB
        KODE = 10
        goto 999
 1044 continue
C----   Type current input file name
        print 119, NAMEAAA
  119   format(' ','Name of current input file is: ',A)
        KODE = 7
        goto 999
 1045 continue
C----   Type additional menu
        KOMM = 2
        KODE = 2
        goto 999
 1046 continue
C----   Change output filter mode
        KODE = 11
        goto 999
 1047 continue
C----   Read more output
        KODE = 12
        goto 999
C
  999 continue
C     !end
      return
      end
      subroutine IS_AT_BEGINNING
     $(NO)
C
C     Rudolf Loeser, 1998 Dec 09
C
C---- Types a message if window currently is at beginning of section
C     !DASH
      save
C     !BDECL
      integer NGR,NLS,NCS,INCS,ISCS,IECS,ICL,ICE,ICC,MODE,MORE,NSC,
     $        NLN,LININC,NOVR
      logical NO
C     !EDECL
C     !COM
      common /control/ NGR,NLS,NCS,INCS,ISCS,IECS,ICL,ICE,ICC,
     $                 MODE,MORE,NSC,NLN,LININC,NOVR
C     !beg
      NO = ICL.ne.ISCS
      if(.not.NO) then
        if(INCS.eq.1) then
          print 100
  100     format(' ','Window is currently at beginning of file.')
        else
          print 101
  101     format(' ','Window is currently at beginning of section; ',
     $               'use "p" to go to preceding section.')
        end if
      end if
C     !end
      return
      end
      subroutine IS_AT_END
     $(NO)
C
C     Rudolf Loeser, 1998 Dec 09
C
C---- Types a message if window currently is at end of section
C     !DASH
      save
C     !BDECL
      integer NGR,NLS,NCS,INCS,ISCS,IECS,ICL,ICE,ICC,MODE,MORE,NSC,
     $        NLN,LININC,NOVR
      logical NO
C     !EDECL
C     !COM
      common /control/ NGR,NLS,NCS,INCS,ISCS,IECS,ICL,ICE,ICC,
     $                 MODE,MORE,NSC,NLN,LININC,NOVR
C     !beg
      NO = ICE.ne.IECS
      if(.not.NO) then
        if(INCS.eq.NSC) then
          print 100
  100     format(' ','Window is currently at end of file.')
        else
          print 101
  101     format(' ','Window is currently at end of section; ',
     $               'use "n" to go to next section.')
        end if
      end if
C     !end
      return
      end
      subroutine IS_CLEAN
     $(LINE,YES,BAD)
C
C     Rudolf Loeser, 2006 Aug 23
C
C---- Determines whether a line from file .aaa is clean, or part of
C     an error/advisory/dump message (i.e. from MESHED/MASHED).
C     !DASH
      save
C     !BDECL
      integer   NSTK,NL
      character LINE*136,NAMLST*10
      logical   YES,CLEAN,PENDG,BAD
C     !EDECL
C     !DASH
      parameter (NSTK=20)
      dimension NAMLST(NSTK)
C
      data CLEAN,PENDG,NL /.true., .false., 0/
C     !beg
      if(PENDG) then
        PENDG = .false.
        CLEAN = .true.
      end if
      if((LINE(2:5).eq.'$%&:').or.(LINE(2:5).eq.'$&%:')) then
        BAD = NL.ge.NSTK
        if(BAD) then
          print 100
  100     format(' ','IS_CLEAN: NAMLST stack is not deep enough.')
          goto 103
        end if
        NL         = NL+1
        NAMLST(NL) = LINE(11:20)
        CLEAN      = .false.
      else if(LINE(2:5).eq.'%&$:') then
        BAD = NL.le.0
        if(BAD) then
          print 101, LINE(:50)
  101     format(' ','IS_CLEAN: unpaired "end" signals: ',A50)
          goto 103
        end if
        if(LINE(11:20).eq.NAMLST(NL)) then
          PENDG = .true.
          NL    = NL-1
        else
          print 102, LINE(11:20), NAMLST(NL), NL
  102     format(' ','IS_CLEAN: unpaired names: ',2A15,I5)
          BAD = .true.
          goto 103
        end if
      end if
  103 continue
      YES = CLEAN
C     !end
      return
      end
      subroutine IS_TOP_OF_PAGE
     $(LINE,YES)
C
C     Rudolf Loeser, 1998 Dec 01
C
C---- Sets YES = .true.  if this is the first line of a "page".
C     !DASH
      save
C     !BDECL
      character FF*1,LINE*(*)
      logical   YES
C     !EDECL
C     !COM
      common /carriage/ FF
C     !beg
      YES = (LINE(:1).eq.FF).or.(LINE(:1).eq.'1')
C     !end
      return
      end
      subroutine LINE_LENGTH
     $(LINE,M,K)
C
C     Rudolf Loeser, 1993 Jul 22
C
C---- Determines "line length" so that trailing blanks are excluded
C     !DASH
      save
C     !BDECL
      integer   M,K
      character LINE*(*)
C     !EDECL
C     !beg
      K = M
  100 continue
        if(LINE(K:K).eq.' ') then
          if(K.gt.1) then
            K = K-1
            goto 100
          end if
        end if
C     !end
      return
      end
      subroutine MOVE_DOWN
     $(OUTPUT,MOVE,KODE,CHANGE)
C
C     Rudolf Loeser, 1998 Nov 25
C
C---- Moves the window down from its current position by at most
C     MOVE lines. The bottom line of the window is not allowed to
C     move over section boundaries.
C     2006 Aug 17: input KODE = 0 means: do not stop at ff
C                             = 1 means: find and stop at ff
C     !DASH
      save
C     !BDECL
      integer   MOVE,NGR,NLS,NCS,INCS,ISCS,IECS,ICL,ICE,ICC,MODE,
     $          MORE,NSC,NLN,LININC,NOVR,IEND,ISTART,jummy,
     $          NEWBOT,NEWTOP,KODE
      character OUTPUT*(*)
      logical   CHANGE,FOUND,YES
C     !EDECL
      common /control/ NGR,NLS,NCS,INCS,ISCS,IECS,ICL,ICE,ICC,
     $                 MODE,MORE,NSC,NLN,LININC,NOVR
C     !DASH
C     !EJECT
      external  MOVE_DOWN_TO_NEXT_PAGE, IS_AT_END, MOVE_TO_BOTTOM,
     $          IS_TOP_OF_PAGE, FIND_FF, MOVE_DOWN_TO_FF
      intrinsic min
C
      dimension OUTPUT(*)
C     !beg
      call IS_AT_END (CHANGE)
      if(CHANGE) then
C----   Does next line have form-feed
        call IS_TOP_OF_PAGE (OUTPUT(ICE+1),YES)
        if(YES) then
C----     OK - just show next page
          call MOVE_DOWN_TO_NEXT_PAGE (OUTPUT,CHANGE)
        else
C----     Set top-line-index
          NEWTOP = ICL+(MOVE-NOVR)
C
          FOUND = .false.
          if(KODE.eq.1) then
C----       Pay attention to page boundaries
            ISTART = min((ICE+1),IECS)
            IEND   = min((NEWTOP+LININC),IECS)
            call FIND_FF (OUTPUT, ISTART,IEND,+1, jummy,FOUND)
          end if
C
          if(FOUND) then
C----       Just set window to end-of-page
            call MOVE_DOWN_TO_FF (OUTPUT,CHANGE)
          else
C----       Set bottom-line-index
            NEWBOT = NEWTOP+LININC
            if(NEWBOT.ge.IECS) then
C----         Moved too far; just set window to end of section
              call MOVE_TO_BOTTOM (OUTPUT,CHANGE)
            else
C----         OK - set parameters for a modified window 
              CHANGE = (NEWTOP.ne.ICL)
              ICL    = NEWTOP
C             ICE    = NEWBOT; changed 2006 Aug 16
              ICE    = 0
            end if
          end if
        end if
      end if
C     !end
      return
      end
      subroutine MOVE_DOWN_TO_FF
     $(OUTPUT,CHANGE)
C
C     Rudolf Loeser, 1998 Dec 07
C
C---- Moves window down to show bottom of current page, i.e. moves the
C     window down so that the line before the next form-feed becomes
C     the last line of the window. The window is not allowed to span
C     form-feeds.
C     !DASH
      save
C     !BDECL
      integer   NGR,NLS,NCS,INCS,ISCS,IECS,ICL,ICE,ICC,MODE,MORE,NSC,
     $          NLN,LININC,NOVR,INDEX,ISTART,NEWBOT,NEWTOP
      character OUTPUT*(*)
      logical   CHANGE,YES,FOUND
C     !EDECL
C     !COM
      common /control/ NGR,NLS,NCS,INCS,ISCS,IECS,ICL,ICE,ICC,
     $                 MODE,MORE,NSC,NLN,LININC,NOVR
C     !DASH
C     !EJECT
      external  FIND_FF, IS_AT_END, IS_TOP_OF_PAGE
      intrinsic min, max
C
      dimension OUTPUT(*)
C     !beg
      call IS_AT_END (CHANGE)
      if(CHANGE) then
C----   Does next line have form-feed?
        call IS_TOP_OF_PAGE (OUTPUT(ICE+1),YES)
        if(YES) then
          print 100
  100     format(' ','Currently at form-feed.')
          CHANGE = .false.
        else
C----     Find next downward form-feed, if any
          ISTART = min((ICE+2),IECS)
          call FIND_FF (OUTPUT, ISTART,IECS,+1, INDEX,FOUND)
          if(.not.FOUND) then
            print 101
  101       format(' ','No form-feed found before end of section.')
            CHANGE = .false.
          else
C----       Set bottom-line-index
            NEWBOT = INDEX-1
C----       Set top-line-index
            NEWTOP = NEWBOT-LININC         
C----       Constrain top-line-index not to overlap previous window
            NEWTOP = max((ICE+1),NEWTOP)
C----       Set parameters for a modified window
            CHANGE = (NEWTOP.ne.ICL).or.(NEWBOT.ne.ICE)
            ICL    = NEWTOP
            ICE    = NEWBOT
          end if
        end if
      end if
C     !end
      return
      end
      subroutine MOVE_DOWN_TO_NEXT_PAGE
     $(OUTPUT,CHANGE)
C
C     Rudolf Loeser, 1998 Dec 01
C
C---- Moves window down to show top of next page, i.e. moves the
C     window down so that next form-feed becomes the first line of
C     the window. The window is not allowed to span form-feeds.
C     !DASH
      save
C     !BDECL
      integer   NGR,NLS,NCS,INCS,ISCS,IECS,ICL,ICE,ICC,MODE,MORE,
     $          NSC,NLN,LININC,NOVR,INDEX,ISTART,NEWBOT,NEWTOP
      character OUTPUT*(*)
      logical   CHANGE,YES,FOUND
C     !EDECL
C     !COM
      common /control/ NGR,NLS,NCS,INCS,ISCS,IECS,ICL,ICE,ICC,
     $                 MODE,MORE,NSC,NLN,LININC,NOVR
C     !DASH
C     !EJECT
      external  FIND_FF, IS_TOP_OF_PAGE, IS_AT_END, FIX_BOTTOM
      intrinsic min
C
      dimension OUTPUT(*)
C     !beg
      call IS_AT_END (CHANGE)
      if(.not.CHANGE) goto 102
C
C---- Does next line have form-feed?
      call IS_TOP_OF_PAGE (OUTPUT(ICE+1),YES)
      if(YES) then
C----   OK, stop right here, and set top-line-index
        NEWTOP = ICE+1
      else
C----   Find next downward form-feed, if any
        ISTART = min((ICE+2),IECS)
        call FIND_FF (OUTPUT, ISTART,IECS,+1, INDEX,FOUND)
        if(.not.FOUND) then
          print 100
  100     format(' ','No form-feed found before end of section.')
          CHANGE = .false.
          goto 102
        else
C----     Set top-line-index
          NEWTOP = INDEX
        end if
      end if
C---- Find bottom-line-index
      NEWBOT = min((NEWTOP+LININC),IECS)
      call FIX_BOTTOM (OUTPUT,NEWTOP,NEWBOT)
C---- Set parameters for a modified window
      CHANGE = (NEWTOP.ne.ICL).or.(NEWBOT.ne.ICE)
      ICL    = NEWTOP
      ICE    = NEWBOT
C
  102 continue
C     !end
      return
      end
      subroutine MOVE_TO_BOTTOM
     $(OUTPUT,CHANGE)
C
C     Rudolf Loeser, 1998 Dec 01
C
C---- Moves the window down to the end of the current section.
C     2006 Aug 15: changed to ignore ff's
C     !DASH
      save
C     !BDECL
      integer   NGR,NLS,NCS,INCS,ISCS,IECS,ICL,ICE,ICC,MODE,MORE,
     $          NSC,NLN,LININC,NOVR,IEND,NEWTOP,NEWBOT
      character OUTPUT*(*)
      logical   CHANGE,lummy
C     !EDECL
C     !COM
      common /control/ NGR,NLS,NCS,INCS,ISCS,IECS,ICL,ICE,ICC,
     $                 MODE,MORE,NSC,NLN,LININC,NOVR
C     !DASH
      external  IS_AT_END
      intrinsic max
C
      dimension OUTPUT(*)
C     !beg
      call IS_AT_END (CHANGE)
      if(CHANGE) then
C----   Set bottom-line-index
        NEWBOT = IECS
C----   Set top-line-index
        NEWTOP = max((NEWBOT-LININC),ISCS)
C----   Set parameters for a modified window
        CHANGE = (NEWTOP.ne.ICL).or.(NEWBOT.ne.ICE)
        ICL    = NEWTOP
        ICE    = NEWBOT
      end if
C     !end
      return
      end
      subroutine MOVE_TO_TOP
     $(OUTPUT,CHANGE)
C
C     Rudolf Loeser, 1998 Dec 01
C
C---- Moves the window up to the beginning of the current section.
C     !DASH
      save
C     !BDECL
      integer   NGR,NLS,NCS,INCS,ISCS,IECS,ICL,ICE,ICC,MODE,MORE,
     $          NSC,NLN,LININC,NOVR,NEWBOT,NEWTOP
      character OUTPUT*(*)
      logical   CHANGE
C     !EDECL
C     !COM
      common /control/ NGR,NLS,NCS,INCS,ISCS,IECS,ICL,ICE,ICC,
     $                 MODE,MORE,NSC,NLN,LININC,NOVR
C     !DASH
      external  IS_AT_BEGINNING, FIX_BOTTOM
      intrinsic min
C
      dimension OUTPUT(*)
C     !beg
      call IS_AT_BEGINNING (CHANGE)
      if(CHANGE) then
C----   Set top-line-index
        NEWTOP = ISCS
C----   Set bottom-line-index
        NEWBOT = min((NEWTOP+LININC),IECS)
C
        call FIX_BOTTOM (OUTPUT,NEWTOP,NEWBOT)
C
C----   Set parameters for a modified window
        CHANGE = (NEWTOP.ne.ICL).or.(NEWBOT.ne.ICE)
        ICL    = NEWTOP
        ICE    = NEWBOT
      end if
C     !end
      return
      end
      subroutine MOVE_UP
     $(OUTPUT,MOVE,KODE,CHANGE)
C
C     Rudolf Loeser, 1998 Nov 30
C
C---- Moves the window up from its current position by at most
C     MOVE lines. The top line of the window is not allowed to move
C     over section boundaries.
C     2006 Aug 17: input KODE = 0 means: do not stop at ff
C                             = 1 means: find and stop at ff
C     !DASH
      save
C     !BDECL
      integer   NGR,NLS,NCS,INCS,ISCS,IECS,ICL,ICE,ICC,MODE,MORE,
     $          NSC,NLN,LININC,NOVR,MOVE,IEND,ISTART,jummy,
     $          NEWBOT,NEWTOP,KODE
      character OUTPUT*(*)
      logical   CHANGE,FOUND
C     !EDECL
C     !COM
      common /control/ NGR,NLS,NCS,INCS,ISCS,IECS,ICL,ICE,ICC,
     $                 MODE,MORE,NSC,NLN,LININC,NOVR
C     !DASH
C     !EJECT
      external  MOVE_TO_TOP, IS_AT_BEGINNING, FIND_FF,
     $          MOVE_UP_TO_FF
      intrinsic max
C
      dimension OUTPUT(*)
C     !beg
      call IS_AT_BEGINNING (CHANGE)
      if(CHANGE) then
C----   Set top-line-index
        NEWTOP = ICL-(MOVE-NOVR)
C
        FOUND = .false.
        if(KODE.eq.1) then
C----     Pay attention to page boundaries
          ISTART = max((ICL-1),ISCS)
          IEND   = max(NEWTOP,ISCS)
          call FIND_FF (OUTPUT, ISTART,IEND,-1, jummy,FOUND)
        end if
C
        if(FOUND) then
C----     Just move to top-of-page
          call MOVE_UP_TO_FF (OUTPUT,CHANGE)
        else
          if(NEWTOP.le.ISCS) then
C----       Moved too far; just show beginning of section
            call MOVE_TO_TOP (OUTPUT,CHANGE)
          else
C----       Set parameters for a modified window
            CHANGE = (NEWTOP.ne.ICL)
            ICL    = NEWTOP
            ICE    = 0
          end if
        end if
      end if
C     !end
      return
      end
      subroutine MOVE_UP_TO_FF
     $(OUTPUT,CHANGE)
C
C     Rudolf Loeser, 1998 Dec 01
C
C---- Moves the window up to show the nearest top-of-page, i.e.
C     moves the window up so that the next form-feed becomes the
C     first line of the window. The window is not allowed to span
C     form-feeds.
C     !DASH
      save
C     !BDECL
      integer   NGR,NLS,NCS,INCS,ISCS,IECS,ICL,ICE,ICC,MODE,MORE,
     $          NSC,NLN,LININC,NOVR,INDEX,ISTART,NEWTOP,NEWBOT
      character OUTPUT*(*)
      logical   CHANGE, FOUND
C     !EDECL
C     !COM
      common /control/ NGR,NLS,NCS,INCS,ISCS,IECS,ICL,ICE,ICC,
     $                 MODE,MORE,NSC,NLN,LININC,NOVR
C     !DASH
      external  FIND_FF, IS_AT_BEGINNING, FIX_BOTTOM
      intrinsic max, min
C
      dimension OUTPUT(*)
C     !beg
      call IS_AT_BEGINNING (CHANGE)
      if(CHANGE) then
        if(ICL.eq.ISCS) then
          print 100
  100     format(' ','Currently at top of form/page.')
          CHANGE = .false.
        else
C----     Find preceding form-feed, if any
          ISTART = max((ICL-1),ISCS)
          call FIND_FF (OUTPUT, ISTART,ISCS,-1, INDEX,FOUND)
          if(.not.FOUND) then
            print 101
  101       format(' ','No form-feed found before beginning ',
     $                 'of section.')
            CHANGE = .false.
          else
C----       Set top-line-index
            NEWTOP = INDEX
C----       Set bottom-line-index
            NEWBOT = min((NEWTOP+LININC),IECS)
            call FIX_BOTTOM (OUTPUT,NEWTOP,NEWBOT)
C----       Set parameters for a modified window
            CHANGE = (NEWTOP.ne.ICL).or.(NEWBOT.ne.ICE)
            ICL    = NEWTOP
            ICE    = NEWBOT
          end if
        end if
      end if
C     !end
      return
      end
      subroutine READ_AAA
     $(NAAA,NAMEAAA,OUTPUT,MXLN,NLN,NLC,BAD,MODE,LINDEX,NSC,MXSC,
     $ JUNK,KPL,KCH,FLL)
C
C     Rudolf Loeser, 1993 Jul 16
C
C---- Reads the entire Pandora output file into memory, and sets up
C     a section index.
C
C     Extensive changes to read very long files in parts, 2006 Aug.
C     !DASH
      save
C     !BDECL
      integer   LINDEX,NAAA,MXLN,NLN,MODE,NSC,MXSC,KPL,NC,NLC,JUNK
      integer   I,KCH,KSC
      character OUTPUT*(*),LINE*136,NAMEAAA*(*),DESCR*12,REM*17
      logical   SECTX,SECTP,SECTF,YES,BAD,FLL
C     !EDECL
C     !DASH
      external  IS_TOP_OF_PAGE, IS_CLEAN, READ_MORE
      intrinsic len
C
      dimension OUTPUT(MXLN,2), DESCR(2), LINDEX(MXSC,2), JUNK(MXSC)
C
      data DESCR /'PSN sections', 'page ejects '/
C     !beg
      NC = len(NAMEAAA)
  100 continue
        if(NAMEAAA(NC:NC).eq.' ') then
          if(NC.gt.1) then
            NC = NC-1
            goto 100
          end if
        end if
C
      print 101, NAMEAAA(:NC)
  101 format(/,' ','Reading ',A,'; please wait...')
C
      if(NLN.gt.0) then
C----   Set up to read the next part of a very long file
        call READ_MORE (LINDEX,MXSC,NSC,KSC)
        KCH = KCH+1
        NLN = 0
        NLC = 0
      else
        KSC = 0
      end if
C
      BAD = .false.
      FLL = .false.
C     !EJECT
  102 continue
        LINE = ' '
        read (NAAA,103,end=105) LINE
  103   format(A)
C
        NLN = NLN+1
        OUTPUT(NLN,1)(:KPL) = LINE
C
        if(.not.BAD) then
          call IS_CLEAN (LINE,YES,BAD)
          if(YES) then
            NLC = NLC+1
            OUTPUT(NLC,2)(:KPL) = LINE
          else
            JUNK(KSC) = 1
          end if
        end if
C
        call IS_TOP_OF_PAGE (LINE,YES)
        SECTF = (MODE.eq.2).and.YES
        SECTX = (LINE(2:4).eq.'PSN')
        SECTP = (MODE.eq.1).and.(SECTX.or.((KCH.eq.1).and.(NLN.eq.1)))
        if(SECTP.or.SECTF) then
          KSC = KSC+1
          if(KSC.gt.MXSC) stop 'READ_AAA: too many sections.'
          JUNK(KSC)     = 0
          LINDEX(KSC,1) = NLN
          LINDEX(KSC,2) = NLC
        end if
C
      FLL = NLN.eq.MXLN
      if(FLL) then
        print 104, NAMEAAA(:NC),KCH
  104   format(' ','File ',A,' is longer than the line buffer, ',
     $             'which now contains part',I4,' of that file.'/
     $         ' ','When finished with this part of the output, ',
     $             'use the command  +  to read more.'/)
        LINDEX(KSC+1,1) = NLN+1
        LINDEX(KSC+1,2) = NLC+1
        goto 107
      else
        goto 102
      end if
C
  105 continue
C     !EJECT
      if(BAD) then
        do 106 I = 1,NSC
          JUNK(I) = 0
  106   continue
      end if
C
  107 continue
      if(FLL) then
        write (REM,108) KCH
  108   format('Read part',I4,' of ')
      else
        REM = 'Finished reading '
      end if
      print 109, REM,NAMEAAA(:NC),NLN,NLC,KSC,DESCR(MODE)
  109 format(' ',A,A,';',I12,' (',I12,') lines,',I5,1X,A)
C     !end
      return
      end
      subroutine READ_AIX
     $(NAIX,NAMEAIX,PSN,PSNNAME,MXSC,NSC)
C
C     Rudolf Loeser, 1993 Jul 13
C     !DASH
      save
C     !BDECL
      integer   NAIX,MXSC,NSC,K
      character PSN*9,PSNNAME*(*),NAMEAIX*(*),LINE*136
C     !EDECL
C     !DASH
      dimension PSN(*), PSNNAME(*)
C     !beg
      K = 0
      if(NAIX.gt.0) then
        K          = 1
        PSN(K)     = 'PSN000000'
        PSNNAME(K) = ' HEADER'
C
        print 100, NAMEAIX
  100   format(/,' ','Reading ',A)
C
        rewind NAIX
  101   continue
          read (NAIX,102,end=103) LINE
  102     format(A)
          if(LINE(2:4).eq.'PSN') then
            K = K+1
            if(K.gt.MXSC) stop 'READ_AIX: index file is too long.'
            PSN(K)     = LINE(2:10)
            PSNNAME(K) = LINE(12:)
          end if
          goto 101
C
  103   continue
        NSC = K
C
        print 104, NSC
  104   format(' ',I5,' index entries were found')
      end if
C     !end
      return
      end
      subroutine READ_MORE
     $(LINDEX,MXSC,NSC,KSC)
C
C     Rudolf Loeser, 2006 Aug 29
C     !DASH
      save
C     !BDECL
      integer LINDEX,MXSC,NSC,KSC,K
C     !EDECL
C     !DASH
      dimension LINDEX(MXSC,2)
C     !beg
      do 100 K = 2,NSC
        if((LINDEX(K-1,1).gt.0).and.(LINDEX(K,1).eq.0)) then
          KSC = K-2
          goto 101
        end if
  100 continue
      stop 'READ_MORE: LINDEX mix-up.'
C
  101 continue
      do 102 K = 1,(KSC-1)
        LINDEX(K,1) = 0
        LINDEX(K,2) = 0
  102 continue
      LINDEX(KSC,1) = 1
      LINDEX(KSC,2) = 1
C     !end
      return
      end
      subroutine SAVE_WINDOW
     $(N)
C
C     Rudolf Loeser
C     !DASH
      save
C     !BDECL
      integer N,KF,KL,LF,LL,KFS,KLS,LFS,LLS,NSI,NSS,NSE,NGR,NLS,NCS,
     $        INCS,ISCS,IECS,ICL,ICE,ICC,MODE,MORE,NSC,NLN,LININC,
     $        NOVR,KVS,KVN
C     !EDECL
C     !COM
      common /control/ NGR,NLS,NCS,INCS,ISCS,IECS,ICL,ICE,ICC,
     $                 MODE,MORE,NSC,NLN,LININC,NOVR
      common /current/ KF,KL,LF,LL,KVN, KFS,KLS,LFS,LLS,NSI,NSS,NSE,KVS
C     !DASH
      dimension KFS(9), KLS(9), LFS(9), LLS(9), NSI(9), NSS(9), NSE(9),
     $          KVS(9)
C     !beg
      if((N.lt.1).or.(N.gt.9)) then
        print 100, N
  100   format(' ','Window ordinal # = ',I10,' is wrong.')
      else
        KFS(N) = KF
        KLS(N) = KL
        LFS(N) = LF
        LLS(N) = LL
        NSI(N) = INCS
        NSS(N) = ISCS
        NSE(N) = IECS
        KVS(N) = KVN
      end if
C     !end
      return
      end
      subroutine SEARCH_BACKWARD
     $(OUTPUT,ISTART,STRING,J)
C
C     Rudolf Loeser, 1994 Sep 12
C
C---- Attempts to find STRING in OUTPUT;
C     sets J equal to the index of the first relevant line in
C     OUTPUT.
C     !DASH
      save
C     !BDECL
      integer   ISTART,J,I,K
      character OUTPUT*(*),STRING*(*)
C     !EDECL
C     !DASH
      intrinsic index
C
      dimension OUTPUT(*)
C     !beg
      do 100 I = ISTART,1,-1
        K = index (OUTPUT(I),STRING)
        if(K.gt.0) then
          J = I
          goto 101
        end if
  100 continue
C
  101 continue
C     !end
      return
      end
      subroutine SEARCH_FORWARD
     $(OUTPUT,ISTART,NLN,STRING,J)
C
C     Rudolf Loeser, 1994 Sep 12
C
C---- Attempts to find STRING in OUTPUT;
C     sets J equal to the index of the first relevant line in
C     OUTPUT.
C     !DASH
      save
C     !BDECL
      integer   ISTART,NLN,J,I,K
      character OUTPUT*(*),STRING*(*)
C     !EDECL
C     !DASH
      intrinsic index
C
      dimension OUTPUT(*)
C     !beg
      do 100 I = ISTART,NLN,+1
        K = index (OUTPUT(I),STRING)
        if(K.gt.0) then
          J = I
          goto 101
        end if
  100 continue
C
  101 continue
C     !end
      return
      end
      subroutine SECT_OUT
     $(NAAS,OUTPUT,KPL,ISCS,IECS)
C
C     Rudolf Loeser, 1993 Jul 16
C     !DASH
      save
C     !BDECL
      integer   NAAS,KPL,ISCS,IECS,I,K
      character OUTPUT*(*),FF*1
C     !EDECL
C     !COM
      common /carriage/ FF
C     !DASH
      external LINE_LENGTH
C
      dimension OUTPUT(*)
C     !beg
      call LINE_LENGTH   (OUTPUT(ISCS),KPL,K)
      write (NAAS,100) FF//OUTPUT(ISCS)(2:K)
  100 format(A)
      do 101 I = (ISCS+1),IECS
        call LINE_LENGTH (OUTPUT(I),KPL,K)
        if(OUTPUT(I)(1:1).eq.'1') then
          write (NAAS,100) FF//OUTPUT(I)(2:K)
        else
          write (NAAS,100) OUTPUT(I)(:K)
        end if
  101 continue
C     !end
      return
      end
      subroutine SET_COLUMNS
     $(NCOL,NSC,NLSE,LCOL)
C
C     Rudolf Loeser, 1998 Nov 24
C     !DASH
      save
C     !BDECL
      integer NCOL,NSC,NLSE,LCOL
C     !EDECL
C     !DASH
      intrinsic min, max
C     !beg
      LCOL = min(max(NCOL,1),3)
      if(NCOL.eq.0) then
        LCOL = 1
        if(NSC.gt.NLSE) then
          LCOL = 2
          if(NSC.gt.(2*NLSE)) then
            LCOL = 3
          end if
        end if
      end if
C     !end
      return
      end
      subroutine SET_LINE
     $(PSN,PSNNAME,LINDEX,J,NLN,NSC,INCS,LINC,LCOL,KVN,JUNK,LINE)
C
C     Rudolf Loeser, 1998 Nov 24
C
C---- Composes a line for TYPE_INDEX.
C     Modified 2006 Aug.
C     !DASH
      save
C     !BDECL
      integer   LINDEX,J,NLN,NSC,INCS,LINC,LCOL,LNAM,JPOS,
     $          I,K,L,JC,JN,LC,KVN,JUNK
      character PSN*(*),PSNNAME*(*),LINE*127,DUMMY*2,E*1
C     !EDECL
C     !DASH
      dimension PSN(*), PSNNAME(*), LINDEX(*), JUNK(*)
C     !beg
      LINE = ' '
      goto (101, 102, 103), LCOL
  101 continue
        LNAM = 108
        JPOS = 1
        goto 100
  102 continue
        LNAM = 15
        JPOS = 60
        goto 100
  103 continue
        LNAM = 15
        JPOS = 40
  100 continue
C     !EJECT
      I = J-LINC
      K = 1-JPOS
      do 105 L=1,LCOL
        I = I+LINC
        if(I.le.NSC) then
          DUMMY = '  '
          if(I.eq.INCS) DUMMY = '**'
          E = ' '
          if((KVN.eq.1).and.(JUNK(I).gt.0)) E = 'm'
C
          JC = LINDEX(I)
          if(I.ge.NSC) then
            JN = NLN+1
          else
            JN = LINDEX(I+1)
          end if
          if((JN.le.0).or.(JC.le.0)) then
            LC = 0
          else
            LC = JN-JC
          end if
C
          K = K+JPOS
          write (LINE(K:),104) PSN(I),LC,E,DUMMY,PSNNAME(I)(:LNAM)
  104     format(' ',A9,I6,A1,A2,A)
        end if
  105 continue
C     !end
      return
      end
      subroutine SET_SECTION
     $(LINDEX,INDEX,CHCK,OK)
C
C     Rudolf Loeser, 1998 Dec 09
C
C---- Sets parameters for a new section. Returns OK = .true. if
C     INDEX is the ordinal of a valid, different section.
C     !DASH
      save
C     !BDECL
      integer LINDEX,INDEX,NGR,NLS,NCS,INCS,ISCS,IECS,ICL,ICE,ICC,
     $        MODE,MORE,NSC,NLN,LININC,NOVR
      logical OK,CHCK
C     !EDECL
C     !COM
      common /control/ NGR,NLS,NCS,INCS,ISCS,IECS,ICL,ICE,ICC,
     $                 MODE,MORE,NSC,NLN,LININC,NOVR
C     !DASH
      dimension LINDEX(*)
C     !beg
      OK = .true.
      if(CHCK.and.(INDEX.eq.INCS)) then
        print 100
  100   format(' ','Window is currently in this section.')
        OK = .false.
      else
        if((INDEX.lt.1).or.(INDEX.gt.NSC)) then
          print 101
  101     format(' ','Invalid section specified.')
          OK = .false.
        else
          INCS = INDEX
          ISCS = LINDEX(INCS)
          if(INCS.lt.NSC) then
            IECS = LINDEX(INCS+1)-1
          else
            IECS = NLN
          end if
        end if
      end if
C     !end
      return
      end
      subroutine SET_WINDOW
     $(KPL,N)
C
C     Rudolf Loeser
C
C---- Sets up current window indices:
C     if N .eq. 0, computes them;
C     if N .ne. 0, uses associated index set N.
C     !DASH
      save
C     !BDECL
      integer KPL,N,M,KF,KL,LF,LL, KFS,KLS,LFS,LLS,NGR,NLS,NCS,
     $        INCS,ISCS,IECS,ICL,ICE,ICC,MODE,MORE,NSC,NLN,LININC,
     $        NOVR,NSI,NSS,NSE,KVS,KVN
      logical JUMP
C     !EDECL
      common /current/ KF,KL,LF,LL,KVN, KFS,KLS,LFS,LLS,NSI,NSS,NSE,KVS
      common /control/ NGR,NLS,NCS,INCS,ISCS,IECS,ICL,ICE,ICC,
     $                 MODE,MORE,NSC,NLN,LININC,NOVR
C     !DASH
      intrinsic min, iabs
C
      dimension KFS(9), KLS(9), LFS(9), LLS(9), NSI(9), NSS(9), NSE(9),
     $          KVS(9)
C     !beg
      if(N.eq.0) then
C----   Compute indices
        if(ICE.eq.0) then
          ICE = min((ICL+LININC),IECS)
        end if
C
        KF = ICC
        KL = min(((KF-1)+NCS),KPL)
        LF = ICL
        LL = ICE
      else
C     !EJECT
        JUMP = N.lt.0
        N    = iabs(N)
C----   Retrieve N'th set
        if((N.lt.1).or.(N.gt.9)) then
          print 100, N
  100     format(' ','Window ordinal # = ',I10,' is wrong.')
          N = -1
        else
          M = KFS(N)+KLS(N)+LFS(N)+LLS(N)
          if(M.le.0) then
            print 101, N
  101       format(' ','Window for ordinal # = ',I1,' is undefined.')
            N = -1
          else
            KF = KFS(N)
            KL = KLS(N)
            LF = LFS(N)
            LL = LLS(N)
            if(JUMP) then
              ICC  = KF
              ICL  = LF
              ICE  = LL
              INCS = NSI(N)
              ISCS = NSS(N)
              IECS = NSE(N)
              KVN  = KVS(N)
            end if
          end if
        end if
      end if
C     !end
      return
      end
      subroutine SIGN_ON
     $(NAAA,NAMEAAA,NAIX,NAMEAIX,NAAS,NAMEOUT,VERSION,MODE,STOP)
C
C     Rudolf Loeser, 1993 Jul 09
C
C---- Starts up a run, and sets up input files.
C     !DASH
      save
C     !BDECL
      real*4    VERSION
      integer   NAAA,NAIX,NAAS,MODE,KOUNT,JP,NC
      character NAMEOUT*(*),NAMEAAA*(*),NAMEAIX*(*),NAME*20,R*1
      logical   STOP
C     !EDECL
C     !DASH
      external  TYPE_STARS
      intrinsic len, index
C     !beg
      NAAA = 0
      NAIX = 0
      NAAS = 0
      MODE = 2
      call TYPE_STARS
      print 100, VERSION
  100 format(' ','Program LOOKAT, Version',F6.2//
     $       ' ','LOOKAT uses a Pandora "output" file, and its ',
     $           'associated "index" file if it exists.'//
     $       ' ','A Pandora "output" file has a name of the form ',
     $           '"name.aaa.nnnn", and an "index" file has a name of ',
     $           'the form "name.aix.nnnn".'/
     $       ' ','The file names you type, now or later, must not ',
     $           'include a directory name; (in other words, these ',
     $           'files should be located in the '/
     $       ' ','current default directory).'//
     $       ' ','Should you request hard-copy output (consisting of ',
     $           'specific portions of the complete Pandora "output" ',
     $           'file),'/
     $       ' ','then a "hardcopy" file, i.e. "name.out.nnnn", will ',
     $           'be created in the current default directory.'/)
      print 101
  101 format(' ','You are being asked, below, to provide "startup ',
     $           'information" for LOOKAT. Please use one of the ',
     $           'following two responses:'/
     $       ' ','  either 1) type the true name of an existing ',
     $           'Pandora "output" file (see above), ',
     $           'and then press <Return>;'/
     $       ' ','  or     2) just press <Return>.'/
     $       ' ','Upon (1), LOOKAT will assume that the associated ',
     $           '"index" file also exists, and will read them both ',
     $           'without further prompting;'/
     $       ' ','upon (2), LOOKAT will prompt you for information ',
     $           'about the "output" and "index" files.'//
     $       ' ','*****  Note that all COMMANDS are lower case, and ',
     $           'that all your responses should conclude with ',
     $           '<Return> (or <Enter>).'//)
C     !EJECT
      print 99
   99 format(' ','Type "startup information": ',$)
      accept 102, NAME
      NC = len(NAME)
      if(NC.gt.0) then
        JP = index(NAME,'.aaa')
        if(JP.gt.0) then
          NAAA = 10
          NAIX = 11
          goto 107
        end if
      end if
C
  102 format(A)
      KOUNT = 0
  103 continue
        KOUNT = KOUNT+1
        print 104
  104   format(' ','Type name of Pandora output file: ',$)
        accept 102, NAME
        NC = len(NAME)
        JP = index(NAME,'.aaa')
        if(JP.gt.0) then
          NAAA = 10
        else
          print 105, NAME
  105     format(' ',A,5X,'This file name does not make sense.'/
     $           ' ','It should consist of a name (of less than 13 ',
     $               'characters), followed by ".aaa."' ,
     $               'and the version number.')
          if(KOUNT.lt.3) then
            goto 103
          else
            STOP = .true.
            goto 999
          end if
        end if
C
      print 106
  106 format(' ','Does the corresponding index file exist (y/n)? ',$)
      accept 102, R
      if(R.eq.'y') then
        NAIX = 11
      end if
C     !EJECT
  107 continue
      if(NAAA.gt.0) then
        NAMEAAA            = NAME(:NC)
        NAMEOUT            = NAMEAAA
        NAMEOUT(JP+1:JP+3) = 'out'
        open (unit=NAAA, file=NAMEAAA, status='OLD', readonly)
      end if
      if(NAIX.gt.0) then
        MODE               = 1
        NAMEAIX            = NAMEAAA
        NAMEAIX(JP+2:JP+3) = 'ix'
        open (unit=NAIX, file=NAMEAIX, status='OLD', readonly)
      end if
C
  999 continue
C     !end
      return
      end
      subroutine SIGN_OFF
     $(NAAA,NAIX,NAAS,NAMEOUT,VERSION)
C
C     Rudolf Loeser, 1993 Jul 12
C
C---- Cleans up after the run.
C     !DASH
      save
C     !BDECL
      real*4    VERSION
      integer   NAAA,NAIX,NAAS
      character NAMEOUT*(*)
C     !EDECL
C     !beg
      close (NAAA)
      if(NAIX.gt.0) then
        close (NAIX)
      end if
      if(NAAS.gt.0) then
        close (NAAS)
        print 100, NAMEOUT
  100   format(' ','**************** The hardcopy output file is: ',A)
      end if
C     !end
      return
      end
      subroutine TYPE_BLANK_LINES
     $(N)
C
C     Rudolf Loeser, 2006 Aug 18
C
C---- Types padding to a screen
C     !DASH
      save
C     !BDECL
      integer I,N
C     !EDECL
C     !DASH
C     !beg
      if(N.gt.0) then
        do 101 I = 1,N
          print 100
  100     format(' ')
  101   continue
      end if
C     !end
      return
      end
      subroutine TYPE_INDEX
     $(PSN,PSNNAME,KVN,LINDEX,JUNK,NCOL)
C
C     Rudolf Loeser, 1993 Jul 20
C
C---- Types the PSN-index
C     !DASH
      save
C     !BDECL
      integer   LINDEX,NCOL,NGR,NLS,NCS,INCS,ISCS,IECS,ICL,ICE,ICC,
     $          MODE,MORE,NSC,NLN,LININC,NOVR,LC,INC,IE,IS,LINC,I,
     $          JF,NL,LCOL,KVN,JUNK,ILS
      character PSN*(*),PSNNAME*(*),DUMMY*1,LINE*127
C     !EDECL
C     !COM
      common /control/ NGR,NLS,NCS,INCS,ISCS,IECS,ICL,ICE,ICC,
     $                 MODE,MORE,NSC,NLN,LININC,NOVR
C     !DASH
C     !EJECT
      external  SET_COLUMNS, SET_LINE, TYPE_BLANK_LINES
      intrinsic min
C
      dimension PSN(*), PSNNAME(*), LINDEX(*), JUNK(*)
C     !beg
      ILS = NLS-2
      call SET_COLUMNS (NCOL,NSC,ILS, LCOL)
      INC = ILS*LCOL
      IE  = 0
  100 continue
        IS   = IE+1
        IE   = min((IE+INC),NSC)
        LINC = (IE-IS+LCOL)/LCOL
C
        call TYPE_BLANK_LINES (1)
        print 101
  101   format(' ',6X,'The flag "m" marks sections that are not ',
     $             '"clean" (i.e. that contain error/advisory/dump ',
     $             'output lines).')
        call TYPE_BLANK_LINES (1)
        JF = IS-1
        NL = 0
        do 103 I = IS,IE,LCOL
          JF = JF+1
          call SET_LINE (PSN,PSNNAME,LINDEX,JF,NLN,NSC,INCS,LINC,
     $                   LCOL,KVN,JUNK,LINE)
          print 102, LINE
  102     format(' ',A)
          NL = NL+1
  103   continue
C
        LC = ILS-NL
        call TYPE_BLANK_LINES (LC)
C
        if(IE.lt.NSC) then
          print 104
  104     format(' ','See more of the index? Type "y" or "n", then ',
     $               '<Return>: ',$)
          accept 105, DUMMY
  105     format(A)
          if(DUMMY.eq.'y') goto 100
        end if
C     !end
      return
      end
      subroutine TYPE_INFO
     $(MODE,LPP,KPW,NLS,NCS,NGR)
C
C     Rudolf Loeser, 1993 Jul 20
C
C---- Types introductory information
C     !DASH
      save
C     !BDECL
      integer MODE,LPP,KPW,NLS,NCS,NGR
C     !EDECL
C     !DASH
      external TYPE_STARS
C     !beg
      call TYPE_STARS
      print 100
  100 format(' ','The LOOKAT program treats the Pandora printout ',
     $           '(file .aaa) as a long scroll that has been unrolled ',
     $           'and fastened in place.'/
     $       ' ','The contents of this scroll are viewed through a ',
     $           '"window" that can be moved about at will.'//
     $       ' ','The printout scroll is divided into "sections". ',
     $           'After a section to be looked at has been specified, ',
     $           'the window can be positioned'/
     $       ' ','anywhere over this section. In addition to looking ',
     $           'at the Pandora printout in this way, the user can ',
     $           'also copy entire sections'/
     $       ' ','to an output file that could later be printed. ',
     $           'A hard copy of the contents of the current window ',
     $           'can also be obtained in this way.'/)
C     !EJECT
      if(MODE.eq.1) then
        print 101
  101   format(' ','Since a Pandora "index" has been provided (i.e. ',
     $             'file .aix, produced when option MAKIX = on), ',
     $             '"sections" begin at PSN-markers'/
     $         ' ','inserted by Pandora. A section to be looked at ',
     $             'must be specified by typing its PSN-number; e.g. ',
     $             'PSN123456 (just as it appears'/
     $         ' ','in the index). The user can request that the ',
     $             'index be displayed on the screen; it is ',
     $             'recommended to have a hardcopy of the index'/
     $         ' ','file available. Note that a PSN-section may ',
     $             'contain one or more lines beginning with the ',
     $             '"page-eject" (form-feed) character.'/
     $         ' ','When such a line falls within the current window, ',
     $             'it is altered so that it does not "start a new ',
     $             'page", and so that every'/
     $         ' ','"blank" character in it is changed to the ',
     $             '"underscore" character.'/)
      else
        print 102
  102   format(' ','All lines in the printout scroll that begin with ',
     $             'the "page-eject" (form-feed) character (i.e. ',
     $             'lines explicitly intended to start'/
     $         ' ','a new page of Pandora''s printout) are considered ',
     $             'first lines of new "sections". A setion to be ',
     $             'looked at must be specified by'/
     $         ' ','typing its page number; e.g. PAGE1234. Jumping ',
     $             'back-and-forth by dead reckoning may be ',
     $             'necessary to find the section of interest.'/
     $         ' ','Use of an "index" file is recommended (Pandora ',
     $             'option MAKIX = on).'/)
      end if
      print 103
  103 format(' ','The window through which a section is viewed is a ',
     $           '"virtual" window that resides in memory. The ',
     $           'height (vertical extent, measured'/
     $       ' ','in units of lines) and width (horizontal extent, ',
     $           'measured in units of characters) of the virtual ',
     $           'window are under user control.'/
     $       ' ','When the contents of the virtual window are ',
     $           'actually displayed on the CRT screen, the ',
     $           'bottom-left character in the virtual window'/
     $       ' ','is displayed at the bottom-left character position ',
     $           'of the "physical" window allocated for use by ',
     $           'LOOKAT.  N o t e  that two lines'/
     $       ' ','of the physical window are preempted for house',
     $           'keeping purposes, and are not available to the ',
     $           'virtual window. If the virtual window'/
     $       ' ','has more lines than the physical window ',
     $           'accommodates, then the contents of the virtual ',
     $           'window will not all fit on the screen,')
C     !EJECT
      print 104, NLS,NCS,LPP,KPW
  104 format(' ','and will be truncated in some way. It is best to ',
     $           'adjust the size of the virtual window so that it ',
     $           'does not exceed the size of the'/
     $       ' ','physical window. Virtual window size can be ',
     $           'changed at any time; the default size is',I4,' ',
     $           'lines by',I4,' characters (maximum allowed'/
     $       ' ','size is',I4,' lines by',I4,' characters).'/)
      print 105, NGR
  105 format(' ','The printout scroll has a "left" edge and a "right" ',
     $           'edge. A given section has a "top" line, a "middle" ',
     $           'line, and a "bottom" line.'/
     $       ' ','The virtual window can be positioned at the top ',
     $           'line, the middle line, the bottom line, the left ',
     $           'edge, or the right edge of a'/
     $       ' ','printout section. When a new section is specified, ',
     $           'the vertical position of the window is ',
     $           'automatically set to the top line of'/
     $       ' ','the new section; however, the horizontal position ',
     $           'of the window with respect to the printout scroll ',
     $           'is left unchanged. The window'/
     $       ' ','can be moved from its current position either up, ',
     $           'down, right, or left, either by one unit, or by a ',
     $           '"group" of units. The size'/
     $       ' ','of a group can be changed at any time; the default ',
     $           'group size is',I3,' units.'/)
      print 106
  106 format(' ','The user interacts with LOOKAT by typing commands ',
     $           'of one or more characters; all commands must be ',
     $           'completed by pressing <Return>.'/
     $       ' ','To see the definitions of all commands, type "h" ',
     $           '(help). To leave the program, type "q" (quit) or ',
     $           '"e" (exit). Typing "h", or '/
     $       ' ','displaying the index, messes up the contents of ',
     $           'the physical window on the CRT screen (so do ',
     $           'messages emitted by the operating'/
     $       ' ','system): typing "rw" (restore window) at any time ',
     $           'causes a fresh copy of the current virtual window ',
     $           'contents to be displayed.'/)
C     !EJECT
      print 107
  107 format(' ','LOOKAT uses two output file filter modes: the ',
     $           '"complete" mode, and the "clean" mode. When ',
     $           '"clean" mode is in effect,'/
     $       ' ','most error messages, advisory messages, and ',
     $           'dumps are NOT displayed in the window. Use the ',
     $           'command "cx" to switch between'/
     $       ' ','output filter modes.'/)
      print 108
  108 format(' ','LOOKAT tries to read the whole output file ',
     $           'into a finite line buffer. Thus a very long file ',
     $           'can only be read in parts.'/
     $       ' ','When you want to look at the next part use ',
     $           'the "+" command to read it into the buffer (thus ',
     $           'erasing the current part).'/
     $       ' ','The only way to return to an earlier part ',
     $           'is to start over.'/)
      print 109
  109 format(' ','Finally, LOOKAT operates in two modes: the "safe" ',
     $           'mode, and the "sorry" mode. Safe mode has just ',
     $           'been described; it is intended'/
     $       ' ','to be friendly and forgiving. In sorry mode: only ',
     $           'one line of the physical window is preempted for ',
     $           'housekeeping purposes; there'/
     $       ' ','is no overlap of window contents when "d" or "u" ',
     $           'are used; the "status & command prompt" line does ',
     $           'not appear at the bottom, and'/
     $       ' ','the null command (i.e. typing nothing before ',
     $           'pressing <Return>) is a shortcut for the command ',
     $           '"d".'/
     $       ' ','Use the command "cm" to switch between modes. ',
     $           'N O T E : "sorry" mode is not recommended for ',
     $           'beginners!'//)
C     !end
      return
      end
      subroutine TYPE_MENU_1
     $(VERSION,NOFF)
C
C     Rudolf Loeser, 1993 Jul 19
C
C---- Types list of main commands
C     !DASH
      save
C     !BDECL
      real*4 VERSION
      integer NOFF
C     !EDECL
C     !DASH
      external TYPE_STARS, TYPE_BLANK_LINES
C     !beg
      call TYPE_STARS
      print 100, VERSION
  100 format(' ','LOOKAT',F6.2,T29,'Main   C O M M A N D S :'/)
      print 101
  101 format(' ','Section Specification',
     $       T29,'PSN######',T39,'Look at this PSN section (###### is ',
     $           'a 6-digit number, just as in the index)'/
     $       T29,'k***',T39,'is an abbreviation equivalent to the ',
     $           'first matching PSN###***,'/
     $       T39,'where *** may be from 1 to 6 digits'/
     $       T29,'page##',T39,'Look at this "page" section (## is ',
     $           'the page number, 1 or more digits)'//
     $       T29,'sf',T39,'Search forward to a target string to be ',
     $           'specified (searching is case-sensitive)'/
     $       T29,'sb',T39,'Search backward to a target string to be ',
     $           'specified (searching is case-sensitive)'//
     $       T29,'n',T39,'Look at the next section'/
     $       T29,'p',T39,'Look at the preceding setion'/)
      print 102
  102 format(' ','Window Positioning',
     $       T29,'t',T39,'Move window up to top'/
     $       T29,'ul',T39,'Move window up 1 line'/
     $       T29,'ug',T39,'Move window up a "group" of lines'/
     $       T29,'u#',T39,'Move window up # window heights, where # ',
     $           'may be at most 1 digit (use "u" for "u1")'/
     $       T29,'uf',T39,'Move window up to next form-feed'//
     $       T29,'m',T39,'Move window to middle'//
     $       T29,'df',T39,'Move window down to next form-feed'/
     $       T29,'d#',T39,'Move window down # window heights, where # '
     $           'may be at most 1 digit (use "d" for "d1")'/
     $       T29,'dg',T39,'Move window down a "group" of lines'/
     $       T29,'dl',T39,'Move window down 1 line'/
     $       T29,'b',T39,'Move window down to bottom'/)
C     !EJECT
      print 103
  103 format(' ','Control parameters',
     $       T29,'cg##',T39,'Change "group" size (## is group size, ',
     $           '1 or more digits)'/
     $       T29,'cw##',T39,'Change virtual window width (## is ',
     $           'number of characters, 1 or more digits)'/
     $       T29,'ch##',T39,'Change virtual window height (## is ',
     $           'number of lines, 1 or more digits)'/
     $       T29,'tv',T39,'Type current parameter values'/)
      print 104
  104 format(' ','Hardcopy output',
     $       T29,'ps',T39,'Copy entire section to output file'/
     $       T29,'pw',T39,'Copy window contents to output file'/)
      print 105
  105 format(' ','Miscellaneous',
     $       T29,'h',T39,'Type this list of Main COMMANDS'/
     $       T29,'a',T39,'Type list of Additional COMMANDS'/
     $       T29,'i#',T39,'Type the PSN-sections index (# = 1,2,3 ',
     $           '(optional) specifies number of columns)'/
     $       T29,'cm',T39,'Reset the operating mode: change from ',
     $           '"safe" to "sorry", or vice versa'/
     $       T29,'cx',T39,'Reset the output filter mode: change from ',
     $           '"complete" to "clean", or vice versa'/
     $       T29,'+',T39,'Read the next part of the PANDORA ',
     $           'output file (overwriting current buffer contents)'//
     $       T29,'rw',T39,'Retype the current window, unchanged ',
     $           '(to refresh a messed-up screen)'//
     $       T29,'q or e',T39,'Quit, or exit from, LOOKAT'//)
C
      call TYPE_BLANK_LINES (13-NOFF)
C     !end
      return
      end
      subroutine TYPE_MENU_2
     $(VERSION,NOFF)
C
C     Rudolf Loeser, 2006 Aug 18
C
C---- Types list of commands
C     !DASH
      save
C     !BDECL
      real*4 VERSION
      integer NOFF
C     !EDECL
C     !DASH
      external TYPE_STARS, TYPE_BLANK_LINES
C     !beg
      call TYPE_STARS
      print 100, VERSION
  100 format(' ','LOOKAT',F6.2,T29,'Additional   C O M M A N D S :'/)
      print 101
  101 format(' ','Window Positioning',
     $       T29,'tl',T39,'Move window up to top-left'/
     $       T29,'bl',T39,'Move window down to bottom-left'//
     $       T27,'le',T39,'Move window left to left edge'/
     $       T27,'lg',T39,'Move window left a "group" of characters'/
     $       T27,'l',T39,'Move window left 1 character'//
     $       T31,'r',T39,'Move window right 1 character'/
     $       T31,'rg',T39,'Move window right a "group" of characters'/
     $       T31,'re',T39,'Move window right to right edge'/)
      print 102
  102 format(T29,'wd#',T39,'Associate the ordinal # with the current ',
     $           'window (# may be at most 1 digit, > 0 )'/
     $       T29,'w#',T39,'Display the window associated with ',
     $           'ordinal # (# may be at most 1 digit, > 0 )'/
     $       T29,'j#',T39,'Jump to the window associated with ',
     $           'ordinal # (# may be at most 1 digit, > 0 )'/)
      print 103
  103 format(' ','Miscellaneous',
     $       T29,'h',T39,'Type list of Main COMMANDS'/
     $       T29,'fn',T39,'Type name of current input file'/
     $       T29,'cf',T39,'Change to new Pandora files'/
     $       T29,'tp',T39,'Type a fiducial (test) pattern'/
     $       T29,'q or e',T39,'Quit, or exit, from LOOKAT'//)
C
      call TYPE_BLANK_LINES (36-NOFF)
C     !end
      return
      end
      subroutine TYPE_PATTERN
     $(LPP)
C
C     Rudolf Loeser, 1993 Aug 11
C
C---- Types a test pattern revealing the size of the physical window.
C     !DASH
      save
C     !BDECL
      integer LPP,J,K
      character NUMB*4
C     !EDECL
C     !beg
      if(LPP.lt.1000) then
        write (NUMB,100) (1000+LPP)
  100   format(I4)
        print 101, NUMB(2:)
  101   format(' ',A3,'-.----1','----.----2','----.----3','----.----4',
     $             '----.----5','----.----6','----.----7','----.----8',
     $             '----.----9','----.----0','----.----1','----.----2',
     $             '----.----3','-')
        K = LPP/10
        do 103 J = K,2,-1
          print 102
  102     format(' ','I'/' ','I'/' ','I'/' ','I'/' ','.'/
     $           ' ','I'/' ','I'/' ','I'/' ','I')
          write (NUMB,100) (1000+(J-1)*10)
          print 101, NUMB(2:)
  103   continue
        print 104
  104   format(' ','I'/' ','I'/' ','I'/' ','I'/' ','.'/
     $         ' ','I'/' ','I'/' ','I')
        write (NUMB,100) 1001
        print 101, NUMB(2:)
      else
        print 105
  105   format(' ','Test Pattern does not work for windows longer ',
     $             'than 1000 lines.')
      end if
C     !end
      return
      end
      subroutine TYPE_STARS
C
C     Rudolf Loeser, 1993 Jul 20
C
C---- Types a line of asterisks
C     !DASH
      save
C     !BDECL
      integer I
      character STARS*33
C     !EDECL
C     !DASH
      data STARS /'*********************************'/
C     !beg
      print 100, (STARS,I=1,4)
  100 format(/,' ',4A33/)
C     !end
      return
      end
      subroutine TYPE_WINDOW
     $(WINDOW,LIND,NL)
C
C     Rudolf Loeser, 1993 Jul 12
C
C---- Types the current screen
C     !DASH
      save
C     !BDECL
      integer   LIND,NGR,NLS,NCS,INCS,ISCS,IECS,ICL,ICE,ICC,MODE,
     $          MORE,NSC,NLN,LININC,NOVR,NLSE,I,NL,K
      character WINDOW*(*)
C     !EDECL
C     !COM
      common /control/ NGR,NLS,NCS,INCS,ISCS,IECS,ICL,ICE,ICC,
     $                 MODE,MORE,NSC,NLN,LININC,NOVR
C     !DASH
      dimension WINDOW(*), LIND(*)
C     !beg
      if(MORE.eq.0) then
        print 100
  100   format(' ',26('WINDO'),'W')
        NLSE = NLS
      else
        NLSE = NLS+1
      end if
C
      do 102 I = 1,NL
        K = LIND(I)
        print 101, WINDOW(I)(:K)
  101   format(A)
  102 continue
C
      K = NLSE-NL
      if(K.gt.0) then
        do 103 I = 1,K
          print 101, ' '
  103   continue
      end if
C     !end
      return
      end
      subroutine WAIT
C
C     Rudolf Loeser, 1998 Dec 07
C     !DASH
      save
C     !BDECL
      character REPLY*1
C     !EDECL
C     !beg
      print 100
  100 format(' ','Please type <Return> to continue.',$)
      accept 101, REPLY
  101 format(A1)
C     !end
      return
      end
      subroutine WINDOW_OUT
     $(NAAS,WINDOW,LIND,N,INCS,PSN,MODE)
C
C     Rudolf Loeser, 1993 Jul 12
C
C---- Writes the current contents of the WINDOW buffer to the
C     hardcopy file
C     !DASH
      save
C     !BDECL
      integer   LIND,I,K,NAAS,N,INCS,MODE
      character WINDOW*(*),PSN*9,FF*1
C     !EDECL
      common /carriage/ FF
C     !DASH
      dimension WINDOW(*), LIND(*), PSN(*)
C     !beg
      if(MODE.eq.1) then
        write (NAAS,100) FF,PSN(INCS)
  100   format(A1,'Copy of window contents; from section ',A9)
      else
        write (NAAS,101) FF,INCS
  101   format(A1,'Copy of window contents; from section ',
     $             '(i.e. page) #',I6)
      end if
      do 103 I = 1,N
        K = LIND(I)
        write (NAAS,102) WINDOW(I)(:K)
  102   format(A)
  103 continue
C     !end
      return
      end
