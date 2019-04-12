      subroutine METAL
C
C     Rudolf Loeser, 1981 May 22
C---- Reads metal data.
C     (This is version 2 of METAL.)
C     !DASH
      save
C     !DASH
      real*8 FLOAT, ZERO, dummy
      integer I, IEL, INT, KERR, KIND, KNAM, KODE, LOOK, LUEO, MODE,
     $        jummy
      character QNAMES*8, QX*8, qummy*8
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
C     !EJECT
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external MACE, KIWI, EMILY, LOOKUC, MESHED, CHLOE, NUTMEG, ABORT,
     $         LINER, UNMIX, CARMEN, HI, BYE
C
      dimension QNAMES(7)
C
      data KNAM   /7/
      data QNAMES /'AB', 'CHI', 'UI', 'UII', 'ATNO', 'LAB', ')'/
C
      call HI ('METAL')
C     !BEG
      KERR = 0
      call MACE
C---- Read, identify and accept element name
      call KIWI     (MODE, dummy, jummy, QX, jummy)
      if(MODE.ne.2) then
        goto 205
      end if
      call UNMIX    (QX)
      call EMILY    (QX(1:2), LUEO, KODE)
      if(KODE.eq.0) then
        call NUTMEG (LUEO, 2)
      end if
      do 100 I = 1,NMT
        if((ELSYM(I).eq.QX(1:3)).or.(ELSYM(I).eq.'   ')) then
          IEL = I
          goto 101
        end if
  100 continue
      goto 200
C
  101 continue
      ELSYM(IEL) = QX(1:3)
C     !EJECT
  102 continue
C----   Read next input field identifier
        call KIWI   (MODE, dummy, jummy, QX, jummy)
        if(MODE.ne.2) then
          goto 201
        end if
        call UNMIX  (QX)
        call LOOKUC (QNAMES, KNAM, QX, KIND, LOOK)
        if(LOOK.ne.1) then
          goto 202
        end if
        if(KIND.eq.KNAM) then
C----     All done
          goto 199
        else if((KIND.LT.1).or.(KIND.gt.(KNAM-1))) then
C----     Bad identifier
          goto 202
        else
C----     Read next numeric field, and move to proper slot
          call KIWI (MODE, FLOAT, INT, qummy, jummy)
          if(KIND.eq.5) then
            if(MODE.ne.3) then
              goto 203
            end if
            LATNO(IEL) = INT
          else
            if(MODE.ne.5) goto 204
            if(KIND.eq.1) then
              ELABD(IEL) = FLOAT
              ELABL(IEL) = ZERO
            else if(KIND.eq.2) then
              ELCHI(IEL) = FLOAT
            else if(KIND.eq.3) then
              ELLU1(IEL) = FLOAT
            else if(KIND.eq.4) then
              ELLU2(IEL) = FLOAT
            else
              ELABL(IEL) = FLOAT
              ELABD(IEL) = ZERO
            end if
          end if
          goto 102
        end if
      continue
C     !EJECT
C---- Process errors
  205 KERR = KERR+1
  204 KERR = KERR+1
  203 KERR = KERR+1
  202 KERR = KERR+1
  201 KERR = KERR+1
  200 continue
      call MESHED  ('METAL', 1)
      write (LUEO,300) (ELSYM(I),I=1,NMT)
  300 format(' ','Trouble reading element data.  List of valid ',
     $           'element names:'//
     $      (' ',10A10))
      if(KERR.eq.0) then
        call LINER (1, LUEO)
        write (LUEO,301) NMT
  301   format(' ','Not more than ',I3,' distinct elements allowed.')
      end if
      call LINER   (1, LUEO)
      write (LUEO,302) QNAMES
  302 format(' ','List of valid control fields:'//
     $      (' ',10A10))
      call CHLOE   (LUEO, QX, KERR)
      call ABORT
      call CARMEN
C----
  199 continue
C     !END
      call BYE ('METAL')
C
      return
      end
