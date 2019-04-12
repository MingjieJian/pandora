      subroutine CARMEN
C
C     Rudolf Loeser, 1996 Dec 10
C---- CARMEN is called during input reading after ABORT,
C     which is nonsense except when "delayed ABORT" is in effect.
C     The point of CARMEN is to get the input reading process back
C     on track by skipping to the nearest point from where it can
C     proceed: the end of the CURRENT input statement (but,
C     it might turn out to be end of the NEXT input statement---
C     can't be helped). However, CARMEN must not skip past "GO";
C     if "GO" is found, then proper action must be taken in the
C     appropriate input reading control routines.
C     !DASH
      save
C     !DASH
      real*8 dummy
      integer LUEO, MODE, jummy
      logical ENDOFST, FOUND
      character QFL*8, QGO*8, QPR*8
C     !COM
C---- CARGOT      as of 1996 Dec 10
      logical     ESCARGO
      common      /CARGOT/ ESCARGO
C     Input reading signal from subroutine CARMEN.
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external KIWI, LINER, NUTMEG, HI, BYE
C
      data QPR,QGO /')', 'GO'/
C
      call HI ('CARMEN')
C     !BEG
      ESCARGO = .false.
C
  100 continue
        call KIWI (MODE, dummy, jummy, QFL, jummy)
        if(MODE.eq.2) then
          ESCARGO = QFL.eq.QGO
          ENDOFST = QFL.eq.QPR
C
          FOUND   = ENDOFST.or.ESCARGO
          if(.not.FOUND) goto 100
C
        else
          goto 100
        end if
      continue
C
      call LINER  (2, LUEO)
      write (LUEO,101)
  101 format(' ','The attempt to reach end-of-statement stopped ',
     $           'on the following input line:')
      call LINER  (1, LUEO)
      call NUTMEG (LUEO, 2)
      call LINER  (1, LUEO)
C     !END
      call BYE ('CARMEN')
C
      return
      end
