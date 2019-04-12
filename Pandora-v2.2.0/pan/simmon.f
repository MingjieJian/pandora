      subroutine SIMMON
C
C     Rudolf Loeser, 1980 Feb 12
C---- Reads 'New Element' data.
C     (This is version 2 of SIMMON.)
C     !DASH
      save
C     !DASH
      real*8 dummy
      integer IRET, J, KERR, LUEO, MODE, jummy
      character QALFA*8, qummy*8
C     !COM
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
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MACE, UNMIX, CARMEN, EMILY, LINER, CHLOE, ABORT, MESHED,
     $         MICE, KIWI, HI, BYE
C     !EJECT
C
      call HI ('SIMMON')
C     !BEG
      KERR = 0
C
      call MACE
C---- Read row index
      call KIWI    (MODE, dummy, J, qummy, jummy)
      if(MODE.ne.3) then
        goto 203
      end if
      if((J.lt.1).or.(J.gt.NMT)) then
        goto 201
      end if
C---- Read and verify element symbol
      call KIWI    (MODE, dummy, jummy, QALFA, jummy)
      if(MODE.ne.2) then
        goto 205
      end if
      call UNMIX   (QALFA)
      call EMILY   (QALFA(1:2), LUEO, IRET)
      if(IRET.le.0) then
        goto 202
      end if
      ELSYM(J) = QALFA
C
C---- Read numeric data
      call KIWI    (MODE, dummy, LATNO(J), qummy, jummy)
      if(MODE.ne.3) goto 203
      call KIWI    (MODE, ELABD(J), jummy, qummy, jummy)
      if(MODE.ne.5) goto 204
      call KIWI    (MODE, ELCHI(J), jummy, qummy, jummy)
      if(MODE.ne.5) goto 204
      call KIWI    (MODE, ELLU1(J), jummy, qummy, jummy)
      if(MODE.ne.5) goto 204
      call KIWI    (MODE, ELLU2(J), jummy, qummy, jummy)
      if(MODE.ne.5) goto 204
      call KIWI    (MODE, ELABL(J), jummy, qummy, jummy)
      if(MODE.ne.5) goto 204
      call KIWI    (MODE, ELDEF(J), jummy, qummy, jummy)
      if(MODE.ne.5) goto 204
      call KIWI    (MODE, dummy, LDEFR(J), qummy, jummy)
      if(MODE.ne.3) then
        goto 203
      end if
C
      call MICE
      goto 199
C     !EJECT
C---- Process errors
  205 KERR = KERR+1
  204 KERR = KERR+1
  203 KERR = KERR+1
  202 KERR = KERR+1
  201 KERR = KERR+1
      call MESHED  ('SIMMON', 1)
      write (LUEO,200)
  200 format(' ','Error reading "NEWELE" data.')
      if(KERR.eq.1) then
        call LINER (1, LUEO)
        write (LUEO,301) NMT,J
  301   format(' ','J is out of range.  NMT=',I3,', J=',I3)
      else if(KERR.eq.2) then
        call LINER (1, LUEO)
        write (LUEO,302) QALFA
  302   format(' ','Unrecognizable element symbol: ',A8)
      else
        call CHLOE (LUEO, QALFA, KERR)
      end if
      call ABORT
      call CARMEN
C
C---- Go home
  199 continue
C     !END
      call BYE ('SIMMON')
C
      return
      end
