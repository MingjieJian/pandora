      subroutine SUBSUR
     $(V,N, VS,NS, DELTA, KODE)
C     Rudolf Loeser, 1979 Apr 18
C---- See remarks in "SUBSET".
C     !DASH
      save
C     !DASH
      real*4 DELTA, V, VS
      integer FLAG, I, J, KODE, N, NS
C     !DASH
      external COMPR
C
      dimension V(*), VS(*)
C
C     !BEG
      KODE = 0
      if(N.gt.0) then
        if(NS.gt.0) then
          do 101 I = 1,NS
            do 100 J = 1,N
              call COMPR (VS(I),V(J),DELTA,FLAG)
              if(FLAG.eq.0) then
                goto 101
              end if
  100       continue
            goto 102
  101     continue
          KODE = 1
        end if
      else
        KODE = -1
      end if
  102 continue
C     !END
C
      return
      end
