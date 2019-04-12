      subroutine RAWOUT
     $(LUN,NR,IOSTAT,ADDRESS,NPHYS,DATA,L)
C     Rudolf Loeser, 1987 Aug 21
C---- Writes one physical record, for RARITE and RARERI.
C     !DASH
      save
C     !DASH
      real*8 DATA
      integer ADDRESS, IOSTAT, L, LUN, NPHYS, NR, RETRY
C     !COM
C---- RARTRY      as of 1998 Jun 02
      integer     LIMTRY
      common      /RARTRY/ LIMTRY
C     Control parameter for RAFAEL (random access I/O).
C     .
C     !DASH
      dimension DATA(L)
C
C     !BEG
      RETRY = 0
  100 continue
      write (LUN, rec=NR, iostat=IOSTAT) ADDRESS,NPHYS,DATA
      if((IOSTAT.ne.0).and.(RETRY.lt.LIMTRY)) then
        RETRY = RETRY+1
        goto 100
      end if
C     !END
C
      return
      end
