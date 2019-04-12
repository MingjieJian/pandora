      subroutine CAVIR
     $(LU,V,N,L,DOIT)
C
C     Rudolf Loeser, 2002 Dec 11
C---- Checks arrays to be printed.
C     !DASH
      save
C     !DASH
      real*8 DELTA, V
      integer KEQ, L, LU, M, N
      logical DOIT, FLAG
C     !DASH
      external NAUGHTD, KONSTD, RANGED
C
      dimension V(N)
C
      data DELTA /1.D-5/
C
C     !BEG
      DOIT = .false.
C
      call NAUGHTD    (V,1,N,FLAG)
      if(FLAG) then
        write (LU,100) N
  100   format(' ',I10,' elements, all equal to zero.')
        goto 999
      end if
C
      if(N.gt.1) then
        M = N-1
        call KONSTD   (V(2),1,M,V(1),FLAG)
        if(FLAG) then
          write (LU,101) N,V(1)
  101     format(' ',I10,' elements, all =',1PE23.15)
          goto 999
        end if
C
        if(L.eq.1) then
          call RANGED (V(2),1,M,DELTA,V(1),KEQ)
          if(KEQ.eq.M) then
            write (LU,102) N,V(1),DELTA
  102       format(' ',I12,' elements, all =',1PE23.15,
     $                 ' to relative tolerance DELTA =',E9.1)
            goto 999
          end if
        end if
      end if
      DOIT = .true.
C
  999 continue
C     !END
C
      return
      end
