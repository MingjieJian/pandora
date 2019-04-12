      subroutine RALENG
     $(LEN,LENGTH,NR8S,GOOD)
C     Rudolf Loeser, 1986 Jul 02
C     (Utility routine for RAFAEL)
C---- Checks whether LEN .gt. 0, and, if yes,
C     sets LENGTH equal to LEN and makes sure that LENGTH is an
C     integral multiple of INTL, namely the smallest integral
C     multiple of INTL that is not less than LEN.
C     Also computes NR8S, the number of data units of type REAL*8
C     that can be stored in LENGTH bytes.
C     !DASH
      save
C     !DASH
      integer INTL, LEN, LENGTH, NR8S
      logical GOOD
C     !DASH
C     (INTL is the number of bytes per data unit of type REAL*8)
      parameter (INTL=8)
C
      intrinsic mod
C
C     !BEG
      GOOD = (LEN.gt.0)
      if(GOOD) then
        LENGTH = LEN
        if(mod(LENGTH,INTL).ne.0) then
          LENGTH = INTL*((LENGTH/INTL)+1)
        end if
        NR8S = LENGTH/INTL
      end if
C     !END
C
      return
      end
