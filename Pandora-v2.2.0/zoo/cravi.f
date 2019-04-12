      subroutine CRAVI
     $(IS,IE,INDS)
C
C     Rudolf Loeser, 2002 Dec 11
C---- Encodes indices for VICAR.
C     !DASH
      save
C     !DASH
      integer IE, IS
      character INDS*12
C     !DASH
C
C     !BEG
      INDS = 'i =    -    '
C
      if(IS.le.9999) then
        write (INDS(4:7),104) IS
      end if
C
      if(IE.le.9) then
        write (INDS(9:12),101) IE
  101   format(I1)
      else if(IE.le.99) then
        write (INDS(9:12),102) IE
  102   format(I2)
      else if(IE.le.999) then
        write (INDS(9:12),103) IE
  103   format(I3)
      else if(IE.le.9999) then
        write (INDS(9:12),104) IE
  104   format(I4)
      end if
C     !END
C
      return
      end
