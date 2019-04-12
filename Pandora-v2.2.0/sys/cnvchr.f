      subroutine CNVCHR
     $(IN,OUT)
C     Rudolf Loeser, 1991 Jun 07
C---- Given the input ASCII character string IN, this routine
C     returns in OUT as much of the left-most portion of it
C     as will fit, with the case of all alphabetic letters changed.
C     !DASH
      save
C     !DASH
      integer I, KODE, L, LI, LO
      character BLANK*1, IN*(*), OUT*(*)
C     !DASH
      intrinsic char, ichar, len, min
C
      data      BLANK /' '/
C
C     !BEG
      LI = len(IN)
      LO = len(OUT)
      L  = min(LI,LO)
      if(L.gt.0) then
        do 100 I=1,L
          KODE = ichar(IN(I:I))
          if((KODE.ge.65).and.(KODE.le.90)) then
            OUT(I:I) = char(KODE+32)
          else if((KODE.ge.97).and.(KODE.le.122)) then
            OUT(I:I) = char(KODE-32)
          else
            OUT(I:I) = IN(I:I)
          end if
  100   continue
        if(LO.gt.L) then
          OUT((L+1):) = BLANK
        end if
      end if
C     !END
C
      return
      end
