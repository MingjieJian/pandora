      subroutine VICIA
     $(CONT,KOPAC,LIST,KNT,TOTAL)
C
C     Rudolf Loeser, 1995 Mar 16
C---- Accumulates eligible Continuum Contributions.
C     (This is version 2 of VICIA.)
C     !DASH
      save
C     !DASH
      real*8 CONT, TOTAL
      integer I, K, KNT, KOPAC, LIST
C     !DASH
      external HI, BYE
C
C               CONT(Nopac), KOPAC(Nopac), LIST(KNT)
      dimension CONT(*),     KOPAC(*),     LIST(*)
C
      call HI ('VICIA')
C     !BEG
      do 100 K = 1,KNT
        I = LIST(K)
        if(KOPAC(I).gt.0) then
          TOTAL = TOTAL+CONT(I)
        end if
  100 continue
C     !END
      call BYE ('VICIA')
C
      return
      end
