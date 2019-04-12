      subroutine YUKUM
     $(MM,LIM,INDEX,KODE,I,LABEL,DUMP,DMPM)
C
C     Rudolf Loeser, 1983 Feb 28
C---- Dump Administrator.
C     (This is version 3 of YUKUM)
C     !DASH
      save
C     !DASH
      integer I, INDEX, KODE, LIM, LUEO, MM
      logical DMPM, DUMP
      character LABEL*100, TYPE*12
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external HALT, LINER, HI, BYE
C
      dimension TYPE(3)
C
      data TYPE /' Shell Rays.', ' Disk Rays. ', ' Mu values. '/
C
      call HI ('YUKUM')
C     !BEG
      if((KODE.lt.1).or.(KODE.gt.3)) then
        write (MSSLIN(1),100) KODE
  100   format('KODE =',I12,', which is neither 1, 2, nor 3.')
        call HALT ('YUKUM',1)
      end if
C
      write (LABEL,101) MM,LIM,TYPE(KODE),I
  101 format('*****',I3,' of ',I3,A12,5X,'I=',I3)
C
      DMPM = DUMP.and.(MM.eq.INDEX)
C
      if(DMPM) then
        call LINER (3, LUEO)
        write (LUEO,102) LABEL
  102   format(' ',A100)
      end if
C     !END
      call BYE ('YUKUM')
C
      return
      end
