      subroutine CNVCHRL
     $(UPPER,LOWER)
C     Rudolf Loeser, 1991 Jun 07
C---- Returns the lower-case ASCII version of
C     an upper-case ASCII letter of the alphabet.
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
      KODE = ichar(UPPER)
      if((KODE.ge.65).and.(KODE.le.90)) then
        LOWER = char(KODE+32)
      else
        LOWER = QUERY
      end if
C     !END
C
      return
      end
