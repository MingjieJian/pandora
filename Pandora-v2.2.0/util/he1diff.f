      program he1diff
C
C     Rudolf Loeser, 2002 Apr 11
C
C---- Extracts PALBET/PBETAL from a ---he1.msc file, for use
C     in the corresponding He-II run during routine operation.
C---- Header line "doctored", 2007 May 07
C     !BDECL
      integer NI,NO,J
      character LINE*80
      logical FOUND
C     !EDECL
C     !DASH
      intrinsic index
      data NI,NO /61, 62/
C     !beg
      FOUND = .false.
C
      rewind NI
  100 continue
        read (NI,101,end=104) LINE
  101   format(A80)
        J = index(LINE(:40),'PALBET')
        if(J.gt.0) then
          J = index(LINE(J+5:40),' start ')
          if(J.le.0) then
            goto 100
          end if
        else
          goto 100
        end if
C
      read (NI,101) LINE
C     (Make sure the header line can be recognized by Ready.)
      LINE(:2) = '> '
      rewind NO  
      write (NO,101) LINE
C
  102 continue
        read (NI,101,end=104) LINE
        if(LINE(:6).ne.'PALBET') then
          goto 102
        end if
C     !EJECT
      FOUND = .true.
      write (NO,101) LINE
  103 continue
        read (NI,101) LINE
        J = index(LINE(:40),'PALBET')
        if(J.gt.0) then
          J = index(LINE(J+5:40),' end ')
          if(J.gt.0) then
            goto 104
          end if
        end if
        write (NO,101) LINE
      goto 103
C
  104 continue
      if(.not.FOUND) then
        write (LINE,105)
  105   format('> EMPTY')
        rewind NO
        write (NO,101) LINE
      end if
      stop 'he1diff: done'
C     !end
      end
