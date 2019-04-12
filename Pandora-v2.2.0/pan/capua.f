      subroutine CAPUA
     $(QNAME)
C
C     Rudolf Loeser, 1973 Jan 15
C---- Reads matrix control switches.
C     !DASH
      save
C     !DASH
      real*8 dummy
      integer KERR, KIND, LOOK, LUEO, MODE, MSW, jummy
      character QNAME*8, QNOME*8, QNSW*8, qummy*8
C     !COM
C---- MATRIX      as of 2006 Sep 06
      integer     PRNSW,EDJSW,KNTIN,KNTED
      real*8      CRITJ,TIMIN,TIMED
      common      /MATRIX1/ PRNSW,EDJSW,KNTIN,KNTED
      common      /MATRIX2/ CRITJ,TIMIN,TIMED
C
C     Control parameters for matrix inversion and determinants.
C
C     PRNSW = 1: print matrix messages; = 0: do not.
C     EDJSW = 1: edit out "junk" using CRITJ; = 0: do not.
C     KNTIN      count of calls to INVERS.
C     KNTED      count of calls to DETERM.
C     CRITJ      "junk" criterion for EDJSW.
C     TIMIN      total time for all matrix inversions.
C     TIMED      total time for all determinants.
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external MACE, LOOKUC, MESHED, CHLOE, ABORT, UNMIX, CARMEN, KIWI,
     $         HI, BYE
C
      dimension QNSW(4)
C
      data MSW  /4/
      data QNSW /'DRPSW', 'EDJSW', 'CRITJ', ')'/
C
      call HI ('CAPUA')
C     !BEG
      KERR = 0
      call MACE
  100 continue
        call KIWI   (MODE, dummy, jummy, QNOME, jummy)
        if(MODE.ne.2) goto 201
C
        call UNMIX  (QNOME)
        call LOOKUC (QNSW, MSW, QNOME, KIND, LOOK)
        if(LOOK.ne.1) goto 202
C
        goto (101, 102, 103, 199 ), KIND
  101   continue
          call KIWI (MODE, dummy, PRNSW, qummy, jummy)
          if(MODE.ne.3) goto 203
          goto 100
  102   continue
          call KIWI (MODE, dummy, EDJSW, qummy, jummy)
          if(MODE.ne.3) goto 203
          goto 100
  103   continue
          call KIWI (MODE, CRITJ, jummy, qummy, jummy)
          if(MODE.ne.5) goto 204
          goto 100
C
C---- Errors
  204 KERR = KERR+1
  203 KERR = KERR+1
  202 KERR = KERR+1
  201 KERR = KERR+1
      call MESHED ('CAPUA', 1)
      write (LUEO,200) QNSW
  200 format(' ','Trouble reading matrix parameters. List of valid ',
     $           'control fields:'//
     $      (' ',5X,10A10))
      call CHLOE  (LUEO, QNOME, KERR)
      call ABORT
      call CARMEN
C
C---- Go home
  199 continue
C     !END
      call BYE ('CAPUA')
C
      return
      end
