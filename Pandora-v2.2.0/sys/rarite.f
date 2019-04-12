      subroutine RARITE
     $(FILDAT,DATA,LENLOG,ADDRESS,GOOD,IOSTAT)
C     Rudolf Loeser, 1986 Jul 02
C---- Write to RA file; see remarks in RAFAEL.
C     !DASH
      save
C     !DASH
      real*8 DATA, FILDAT, ONE, TWO, XLEN
      integer ADDRESS, IE, IOSTAT, IPHYS, IS, LENGTH, LENLOG, LUN, NEXT,
     $        NPHYS, NR8S
      logical GOOD
C     !DASH
      external  RALENG, RAWOUT
      intrinsic min
C
      dimension DATA(*),FILDAT(11)
C
      data      ONE, TWO /1.D0, 2.D0/
C
C     !BEG
      call RALENG (LENLOG,LENGTH,NR8S,GOOD)
      if(GOOD) then
        LUN     = FILDAT(1)
        XLEN    = LENGTH
        ADDRESS = FILDAT(2)
        IPHYS   = FILDAT(4)/TWO
        NPHYS   = 0
        IE      = 0
  100   continue
          IS = IE+1
          IE = min((IE+IPHYS),NR8S)
          NPHYS = NPHYS+1
          NEXT  = FILDAT(2)
C
          call RAWOUT (LUN,NEXT,IOSTAT,
     $                 ADDRESS,NPHYS,DATA(IS),(IE-IS+1))
C
          GOOD = (IOSTAT.eq.0)
          if(GOOD) then
            FILDAT(2) = FILDAT(2)+ONE
            FILDAT(5) = FILDAT(5)+ONE
            if(IE.lt.NR8S) goto 100
          end if
          FILDAT(6) = FILDAT(6)+ONE
          FILDAT(7) = FILDAT(7)+XLEN
      end if
C     !END
C
      return
      end
