      subroutine RIVAC
     $(LU,MORE,LINE,PINE,LO,LN)
C
C     Rudolf Loeser, 2002 Dec 11
C---- Prints standard vector and array output.
C     !DASH
      save
C     !DASH
      integer LN, LO, LU
      logical MORE
      character LINE*127, PINE*127
C     !DASH
      external LINER
C
C     !BEG
      if(MORE.and.(LINE(13:).eq.PINE(13:))) then
        LO = LO+1
      else
C
        if(LO.gt.0) then
          if(LO.eq.1) then
            write (LU,100) PINE
  100       format(' ',A)
          else
            write (LU,101) LO
  101       format(' ',10X,I5,' lines not printed: same as previous.')
          end if
          LN = LN+1
          LO = 0
        end if
C
        write (LU,100) LINE
        LN = LN+1
      end if
C
      if((LN.ge.5).and.MORE) then
        call LINER (1,LU)
        LN = 0
      end if
C     !END
C
      return
      end
