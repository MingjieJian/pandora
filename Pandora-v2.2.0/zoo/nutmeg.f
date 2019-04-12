      subroutine NUTMEG
     $(NO,KODE)
C
C     Rudolf Loeser, 1983 Oct 11
C---- Writes, to unit NO, the current contents of
C     the line buffer of NUDEAL.
C     If KODE=1, then just the buffer contents are printed;
C     if KODE=2, then character position labels, and
C     the position of the Character Pointer, are also printed.
C     (This is version 4 of NUTMEG.)
C     !DASH
      save
C     !DASH
      integer I, KODE, LAST, NO
      character BLANK*1, BUFFER*80, STAR*1
C     !COM
      common /MAORI/ BUFFER
      common /KAURI/ LAST
C     !DASH
      external LINER
C
      data BLANK,STAR /' ', '*'/
C
C     !BEG
      if(NO.gt.0) then
        call LINER (3, NO)
C
        if(KODE.eq.2) then
C----     Print character position labels
          write (NO,100) (I,I=10,80,10)
  100     format(' ',8I10/
     $           ' ',8('....-....+'))
        end if
C
C----   Print buffer
        write (NO,101) BUFFER
  101   format(' ',A80)
C
        if(KODE.eq.2) then
C----     Print indicator of position of Character Pointer
          write (NO,102) (BLANK,I=1,LAST-1),STAR
  102     format(' ',80A1)
        end if
      end if
C     !END
C
      return
      end
