      subroutine NUFILD
     $(KODE,BUFFER,LAST,FIELD,NCAR)
C     Rudolf Loeser, 1978 Nov 29
C---- Sets up the next input field, in FIELD, and
C     returns the character count in NCAR.
C     !DASH
      save
C     !DASH
      integer IPR, IRD, KARD, KART, KODE, LAST, LIDKNT, LUIN, LUOUT,
     $        MAXCAR, MAXLID, NCAR
      character BLANK*1, BUFFER*80, CHAR*1, ESC*1, FIELD*61, MINUS*1,
     $          PLUS*1, POINT*1, XF*1
C     !COM
      common /NUDEEL/ LUIN,IRD,IPR,LUOUT,KARD,KART
      common /NUCONT/ BLANK,POINT,PLUS,MINUS,ESC,XF
C     !DASH
      external NUCHAR
C
      dimension XF(4)
C
      data MAXLID, MAXCAR /159, 60/
C     !EJECT
C
C     !BEG
      KODE = 0
C---- Initialize leading-blanks-counter
      LIDKNT = 0
  100 continue
C----   Read, and skip over leading blanks
        call NUCHAR     (BUFFER,LAST,CHAR)
        if(CHAR.eq.BLANK) then
          LIDKNT = LIDKNT+1
          if(LIDKNT.le.MAXLID) then
C----       Next character
            goto 100
          else
C----       Too many leading blanks - set signal, and go out
            KODE = 1
          end if
        else if(CHAR.eq.ESC) then
C----     Set signal to force reading a new line, and get
C         next character
          IRD = 1
          goto 100
        else
C----     Found first non-blank - accept it
          NCAR = 1
          FIELD(NCAR:NCAR) = CHAR
C
  101     continue
C----       Read, and accept all contiguous non-blanks,
C           as well as the terminating blank
            call NUCHAR (BUFFER,LAST,CHAR)
            NCAR = NCAR+1
            if(NCAR.gt.MAXCAR) then
C----         Field too long - skip the rest
              KODE = 4
            else
              FIELD(NCAR:NCAR) = CHAR
              if(CHAR.ne.BLANK) then
                goto 101
              end if
            end if
        end if
C     !END
C
      return
      end
