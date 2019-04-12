      subroutine SUBSSD
     $(V,N, VS,NS, DELTA, KODE)
C     Rudolf Loeser, 1979 Apr 18
C---- See remarks in "SUBSET".
C     !DASH
      save
C     !DASH
      real*8 DELTA, DIF, V, VS
      integer FLAG, I, IS, J, JS, KDIST, KODE, KSORT, KSORTS, N, NS
C     !COM
      common /JUDGD/ DIF
C     !DASH
      external JUDGED, SORTDD, COMPD
C
      dimension V(*), VS(*)
C
C     !BEG
      KODE = 0
      if(N.gt.0) then
        if(NS.gt.0) then
          DIF = DELTA
          call SORTDD        (V ,N ,JUDGED,KSORT ,KDIST)
          call SORTDD        (VS,NS,JUDGED,KSORTS,KDIST)
          if(KSORT.ne.0) then
            if(KSORT.eq.KSORTS) then
              IS = 1
              do 102 I = 1,NS
                do 100 J = IS,N
                  JS = J
                  call COMPD (VS(I),V(J),DELTA,FLAG)
                  if(FLAG.eq.0) goto 101
  100           continue
                goto 103
  101           continue
                IS = JS
  102         continue
              KODE = 1
            end if
          else
            KODE = -2
          end if
        end if
      else
        KODE = -1
      end if
  103 continue
C     !END
C
      return
      end
