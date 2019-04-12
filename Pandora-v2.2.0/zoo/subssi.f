      subroutine SUBSSI
     $(V,N, VS,NS, DELTA, KODE)
C     Rudolf Loeser, 1979 Apr 18
C---- See remarks in "SUBSET".
C     !DASH
      save
C     !DASH
      integer DELTA, DIF, FLAG, I, IS, J, JS, KDIST, KODE, KSORT,
     $        KSORTS, N, NS, V, VS
C     !COM
      common /JUDGI/ DIF
C     !DASH
      external JUDGEI, SORTDI, COMPI
C
      dimension V(*), VS(*)
C
C     !BEG
      KODE = 0
      if(N.gt.0) then
        if(NS.gt.0) then
          DIF = DELTA
          call SORTDI        (V ,N ,JUDGEI,KSORT ,KDIST)
          call SORTDI        (VS,NS,JUDGEI,KSORTS,KDIST)
          if(KSORT.ne.0) then
            if(KSORT.eq.KSORTS) then
              IS = 1
              do 102 I = 1,NS
                do 100 J = IS,N
                  JS = J
                  call COMPI (VS(I),V(J),DELTA,FLAG)
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
