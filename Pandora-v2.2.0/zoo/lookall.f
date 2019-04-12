      subroutine LOOKALL
     $(TABLE,LEN, X,DELTA, INDXF,INDXL)
C
C     Rudolf Loeser, 1995 Apr 12
C
C---- LOOKALL is a special table lookup routine.
C
C     Given the table TABLE, of length LEN, sorted in ascending order,
C     with one or more occurrences of two or more elements having
C     the "same" value.
C     LOOKALL expects to find X in this table, and returns indices
C     INDXF and INDXL such that these are the ordinals of the first
C     and the last occurrence, respectively, of X in TABLE.
C     Returns  0,0  if X is not in TABLE.
C     !DASH
      save
C     !DASH
      real*8 DELTA, TABLE, X
      integer INDXF, INDXL, ISIG, K, LEN, LOOK, NOTE
C     !DASH
      external LOOKSD, COMPD, HI, BYE
C
C               TABLE(LEN)
      dimension TABLE(*)
C
C     !BEG
      INDXF = 0
      INDXL = 0
      call LOOKSD (TABLE,LEN, DELTA,X, K,NOTE,LOOK)
      if(LOOK.eq.2) then
        K = LEN
      end if
C
      if(((LOOK.eq.1).and.(NOTE.eq.1)).or.(LOOK.eq.2)) then
        INDXF = K
        INDXL = K
C
  100   continue
          if(INDXF.gt.1) then
            call COMPD (TABLE(INDXF-1),X,DELTA,ISIG)
            if(ISIG.eq.0) then
              INDXF = INDXF-1
              goto 100
            end if
          end if
  101   continue
          if(INDXL.lt.LEN) then
            call COMPD (TABLE(INDXL+1),X,DELTA,ISIG)
            if(ISIG.eq.0) then
              INDXL = INDXL+1
              goto 101
            end if
          end if
      end if
C     !END
C
      return
      end
