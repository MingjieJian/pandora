      subroutine KIWI
     $(MODE,FLPT,INT,ALF,NCAR)
C     Rudolf Loeser, 1983 Oct 11
C---- Uses NUDEAL to obtain the next free-field input field.
C     KIWI ignores "comment fields":
C     beginning with "[ ", and ending with "] ", inclusive.
C---- Upon return, KIWI sets MODE to indicate the type of field found:
C     MODE=1 - empty field;
C         =2 - alphanumeric field, value returned in ALF (character);
C         =3 - integer field, value returned in INT (integer*4);
C         =4 - excessive field; and
C         =5 - floating point field, value returned in FLPT (real*8).
C     NCAR is the number of characters in the field as read
C     (including the terminating break character).
C     (This is version 3 of KIWI.)
C     !DASH
      save
C     !DASH
      real*8 FLPT, dummy
      integer INT, MODE, NCAR, jummy
      character ALF*(*), LBRAK*1, RBRAK*1
C     !COM
C---- DATAFIL     as of 1984 Apr 19
      integer     KIWILFN
      common      /DATAFIL/ KIWILFN
C     Number of unit from which to read input statements.
C     .
C---- CARDFIL     as of 1984 Apr 19
      integer     KIWIOUT
      common      /CARDFIL/ KIWIOUT
C     Number of unit to which to copy input statements.
C     .
C---- INCARD      as of 1984 Apr 23
      integer     LAST,LUINP,IRD,IPR,LUOUT,KARD,KART
      character   BUFFER*80
      common      /MAORI/  BUFFER
      common      /KAURI/  LAST
      common      /NUDEEL/ LUINP,IRD,IPR,LUOUT,KARD,KART
C     Storage and controls for reading input statements:
C     BUFFER - input line buffer;
C     LAST   - input buffer scan pointer;
C     LUINP,IRD,IPR,LUOUT,KARD,KART - "NUDEAL" control
C     parameters, q.v.
C     .
C     !DASH
      external  NUDEAL
C
      data LBRAK,RBRAK /'[', ']'/
C     !EJECT
C
C     !BEG
C---- Set up output unit
      if(KIWIOUT.eq.0) then
        IPR = 0
      else
        IPR = 1
        if(LUOUT.ne.KIWIOUT) then
          LUOUT = KIWIOUT
        end if
      end if
C
C---- Set up current input unit, and its line counter
      if(KIWILFN.ne.LUINP) then
        LUINP = KIWILFN
        IRD   = 1
        KARD  = 0
      end if
C
C---- Read next field
  100 continue
      call NUDEAL   (MODE,FLPT,INT,ALF,NCAR,BUFFER,LAST)
      if((MODE.eq.2).and.(ALF(1:1).eq.LBRAK)) then
C----   Skip over comment, and read following field
  101   continue
        call NUDEAL (MODE,dummy,jummy,ALF,jummy,BUFFER,LAST)
        if(.not.((MODE.eq.2).and.(ALF(1:1).eq.RBRAK))) then
          goto 101
        end if
        goto 100
      end if
C     !END
C
      return
      end
