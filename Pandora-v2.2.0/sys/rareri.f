      subroutine RARERI
     $(FILDAT,DATA,LENLOG,ADDRESS,GOOD,IOSTAT)
C     Rudolf Loeser, 1986 Jul 02
C---- Rewrite to RA file; see remarks in RAFAEL.
C     !DASH
      save
C     !DASH
      real*8 DATA, FILDAT, ONE, TWO, XLEN
      integer ADDRESS, IE, IOSTAT, IPHYS, IS, LENGTH, LENLOG, LUN,
     $        NPHYS, NR8S, RECNUM
      logical GOOD
C     !DASH
      external  RALENG, RAWOUT
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
          call RAWOUT (LUN,RECNUM,IOSTAT,
     $                 ADDRESS,NPHYS,DATA(IS),(IE-IS+1))
C
          GOOD = (IOSTAT.eq.0)
          if(GOOD) then
            RECNUM = RECNUM+1
            if(IE.lt.NR8S) goto 100
          end if
          FILDAT(8) = FILDAT(8)+ONE
          FILDAT(9) = FILDAT(9)+XLEN
      end if
C     !END
C
      return
      end
