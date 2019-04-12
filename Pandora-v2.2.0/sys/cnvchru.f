      subroutine CNVCHRU
     $(LOWER,UPPER)
C     Rudolf Loeser, 1991 Jun 07
C---- Returns the upper-case ASCII version of
C     a lower-case ASCII letter of the alphabet.
C     !DASH
      save
C     !DASH
      integer KODE
      character LOWER*1, QUERY*1, UPPER*1
C     !DASH
      intrinsic char, ichar
C
      data      QUERY /'?'/
C
C     !BEG
      KODE = ichar(LOWER)
      if((KODE.ge.97).and.(KODE.le.122)) then
        UPPER = char(KODE-32)
      else
        UPPER = QUERY
      end if
C     !END
C
      return
      end
