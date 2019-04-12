      subroutine NUDEAL
     $(KODE,FLOTE,INTGR,ALPHA,NCAR,BUFFER,LAST)
C     Rudolf Loeser, 1978 Nov 29
C     (Revised, re. exponent field flags, 1996 Dec 06.)
C
C---- Free-field input routine, returning the next input field.
C
C     BUFFER (80 characters) and LAST are storage for intermediate
C     data, dealing with the scanning of a particular line.
C     LAST is a pointer designating that character in BUFFER which
C     was last scanned.
C
C---- Upon return, if
C
C     KODE=1, then the field is  e m p t y  (more than 159 successive
C             blanks occurred without anything else turning up.)
C     KODE=2, then the field is  a l p h a n u m e r i c,
C             NCAR characters long (including the terminating blank),
C             and as much of it as fits in ALPHA (CHARACTER) is
C             returned, (left justified, with blank fill if needed).
C     KODE=3, then the field is  i n t e g e r,  and its value is
C             returned in INTGR (INTEGER), (if the field contained
C             more than 9 significant digits, then it is considered
C             alphanumeric).
C     KODE=4, then the field is  t o o  l o n g  (longer than 60
C             characters).
C     KODE=5, then the field is a  f l o a t i n g  point number,
C             and its value is returned in FLOTE (REAL*8),
C             (if the field contained more than 16 significant
C             digits, or if the absolute value of the exponent is
C             larger than 300, then the field is considered
C             alphanumeric).
C
C---- In common NUDEEL:
C
C     LUIN   is the logical input unit number.
C     IRD    =1 means - read new card image into buffer before
C            beginning to scan (is reset to 0 after reading).
C     LUOUT  is a logical output unit number.
C     IPR    =1 means - print each new line on LUOUT, as it is read.
C     KART   total number of lines read.
C     KARD   number of lines read since last change of logical
C            input unit.
C     !EJECT
C     !DASH
      save
C     !DASH
      real*8 EXM, FL, FLOTE, FONE, FR, FTEN, FZERO, S, TERM, V
      integer I, INTGR, IPR, IRD, KARD, KART, KODE, LAST, LUIN, LUOUT,
     $        MXF, MXI, N, NCAR, NINE, POWER, STATE, ZERO
      logical LFPT, LINT
      character ALPHA*(*), BLANK*1, BUFFER*80, ESC*1, FIELD*61, MINUS*1,
     $          PLUS*1, POINT*1, XF*1
C     !COM
      common /NUDEEL/ LUIN,IRD,IPR,LUOUT,KARD,KART
      common /NUCONT/ BLANK,POINT,PLUS,MINUS,ESC,XF
      common /NUASCI/ ZERO,NINE
C     !DASH
      external  NUFILD, NUFSA
      intrinsic sign
C
      dimension S(3), V(3), N(3), XF(4)
C              (S(2)    and N(3) are not used)
C
      data FZERO, FONE, FTEN /0.D0, 1.D0, 1.D1/
      data EXM /3.D2/
      data MXI, MXF /9, 16/
      data BLANK, POINT, PLUS, MINUS, ESC /' ', '.', '+', '-', '>'/
      data XF /'e', 'E', 'd', 'D'/
      data ZERO,NINE /48, 57/
C     !EJECT
C
C     !BEG
      STATE = 5
      do 100 I = 1,3
        S(I) = FONE
        V(I) = FZERO
        N(I) = 0
  100 continue
      ALPHA = BLANK
      FLOTE = FZERO
      INTGR = 0
C
C---- Get next field
      call NUFILD       (KODE,BUFFER,LAST,FIELD,NCAR)
      if(KODE.eq.0) then
C----   Decode the field character-by-character, leaving results
C       in N, V, and S
        call NUFSA      (STATE,FIELD,NCAR,N,V,S)
C----   Final processing
        LINT = (STATE.eq.2).and.(N(1).gt.0).and.(N(1).le.MXI)
        LFPT = ((STATE.eq.3).or.(STATE.eq.4)).and.(V(3).le.EXM).and.
     $         ((N(1)+N(2)).gt.0).and.((N(1)+N(2)).le.MXF)
        if(LINT) then
C----     Integer field
          TERM  = sign(V(1),S(1))
          INTGR = TERM
          KODE  = 3
        else if(LFPT) then
C----     Floating point field
          POWER = sign(V(3),S(3))
          FL    = FTEN**(POWER     )
          FR    = FTEN**(POWER-N(2))
          TERM  = V(1)*FL+V(2)*FR
          FLOTE = sign(TERM,S(1))
          KODE  = 5
        else
C----     Alphanumeric field
          ALPHA = FIELD(1:NCAR)
          KODE  = 2
        end if
      end if
C     !END
C
      return
      end
