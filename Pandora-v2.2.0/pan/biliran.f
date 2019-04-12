      subroutine BILIRAN
     $(NO,RCDNAM,INDADR,INDLEN,INDLIM,RNAME,DELTA)
C
C     Rudolf Loeser, 1986 Jul 08
C---- Prints a scratch I/O record index, and search data.
C     !DASH
      save
C     !DASH
      real*8 DELTA, RCDNAM, RNAME
      integer I, INDADR, INDLEN, INDLIM, NO
C     !DASH
      external LINER, HI, BYE
C
C               RCDNAM(INDLEN), INDADR(INDLEN)
      dimension RCDNAM(*),      INDADR(*)
C
      call HI ('BILIRAN')
C     !BEG
      if(NO.gt.0) then
        write (NO,100) RNAME,DELTA
  100   format(' ','Scratch I/O record search data and record index.'//
     $         ' ','RNAME =',1PE24.16,10X,'DELTA =',E12.4)
        if(INDLIM.gt.0) then
          call LINER (1,NO)
          write (NO,101) INDLIM
  101     format(' ','INDLIM =',I24)
        end if
        call LINER   (1,NO)
        write (NO,102) INDLEN
  102   format(' ','INDLEN =',I24)
        if(INDLEN.gt.0) then
          call LINER (2,NO)
          write (NO,103)
  103     format(' ',24X,'RCDNAM',10X,'INDADR')
          call LINER (1,NO)
          write (NO,104) (I,RCDNAM(I),INDADR(I),I=1,INDLEN)
  104     format(5(' ',I6,1PE24.16,I16/))
        end if
      end if
C     !END
      call BYE ('BILIRAN')
C
      return
      end
