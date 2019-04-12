      subroutine ETTA
     $(LABEL,TITLE,KERM,NERM,KMSS,NORMAL,ERROR)
C
C     Rudolf Loeser, 1990 Jun 29
C---- Sets up error message paraphernalia, for EDITH.
C
C---- Length of LABEL should not exceed 100 characters.
C
C     (This is version 2 of ETTA.)
C     !DASH
      save
C     !DASH
      integer K, KERM, KMSS, NERM
      logical ERROR, NORMAL
      character LABEL*(*), TITLE*127
C     !DASH
      external  HI, BYE
      intrinsic min, len
C
      call HI ('ETTA')
C     !BEG
      K = len(LABEL)
      K = min(K,100)
      TITLE = LABEL(:K)
      write (TITLE(101:127),100) KERM,NERM
  100 format(' [mssg #',I7,'/max',I7,']')
C
      NORMAL = .false.
      ERROR  = .false.
      if(KMSS.gt.0) then
        if(NERM.lt.0) then
          ERROR  = .true.
        else if(KERM.le.NERM) then
          NORMAL = .true.
        end if
      end if
C     !END
      call BYE ('ETTA')
C
      return
      end
