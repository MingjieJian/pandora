      subroutine CHLOE
     $(NO,NAME,K)
C     Rudolf Loeser, 1983 Oct 03
C---- Error message printer for NUDEAL-based readers.
C     (This is version 2 of CHLOE.)
C     !DASH
      save
C     !DASH
      integer K, NO
      character MESS*60, NAME*8
C     !DASH
      external NUTMEG, LINER
C
      dimension MESS(10)
C
      data MESS /
C  1
     $ 'Control field is not alphanumeric.',
C  2
     $ 'Unrecognizable control field. See list above.',
C  3
     $ 'Integer field expected but not found.',
C  4
     $ 'Floating point field expected but not found.',
C  5
     $ 'Alphanumeric field expected but not found.',
C  6
     $ 'Control field expected but not found. See list above.',
C  7
     $ 'Empty field - too many break characters.',
C  8
     $ 'Excessively long field.',
C  9
     $ 'This field does not make sense.',
C 10
     $ 'Too many data fields.'/
C
C     !BEG
      if(NO.gt.0) then
        call LINER   (1,NO)
        write (NO,100) NAME
  100   format(' ','Current input field: "',A8,'"')
        call NUTMEG  (NO,2)
        if((K.ge.1).and.(K.le.10)) then
          call LINER (1,NO)
          write (NO,101) MESS(K)
  101     format(' ',A60)
        end if
      end if
C     !END
C
      return
      end
