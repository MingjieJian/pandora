      subroutine RAREAD
     $(FILDAT,DATA,LENLOG,ADDRESS,GOOD,IOSTAT)
C     Rudolf Loeser, 1986 Jul 02
C---- Read from RA file; see remarks in RAFAEL.
C     !DASH
      save
C     !DASH
      real*8 DATA, FILDAT, ONE, TWO, XLEN
      integer ADDRESS, FIRST, IE, IOSTAT, IPHYS, IS, LENGTH, LENLOG,
     $        LUN, MPHYS, NPHYS, NR8S, RECNUM
      logical FOUND, GOOD
C     !DASH
      external  RALENG, RAWINN
      intrinsic min
C
      dimension DATA(*), FILDAT(11)
C
      data      ONE, TWO /1.D0, 2.D0/
C
C     !BEG
      call RALENG (LENLOG,LENGTH,NR8S,GOOD)
      if(GOOD) then
        LUN    = FILDAT(1)
        XLEN   = LENGTH
        RECNUM = ADDRESS
        IPHYS  = FILDAT(4)/TWO
        NPHYS  = 0
C
        IE     = 0
  100   continue
          IS = IE+1
          IE = min((IE+IPHYS),NR8S)
          NPHYS = NPHYS+1
C
          call RAWINN (LUN,RECNUM,IOSTAT,
     $                 FIRST,MPHYS,DATA(IS),(IE-IS+1))
C
          FOUND = (FIRST.eq.ADDRESS).and.(MPHYS.eq.NPHYS)
          GOOD  = (IOSTAT.eq.0).and.FOUND
          if(GOOD) then
            RECNUM = RECNUM+1
            if(IE.lt.NR8S) goto 100
          end if
          FILDAT(10) = FILDAT(10)+ONE
          FILDAT(11) = FILDAT(11)+XLEN
      end if
C     !END
C
      return
      end
