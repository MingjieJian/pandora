      subroutine SORT
     $(A,N,INDX,NAME)
C
C     Rudolf Loeser, 2003 Aug 08
C---- Sorts an array (with its associated pointer array), but
C     aborts with an error message if sorting fails.
C
C     See also: SORTI and SORTC.
C     !DASH
      save
C     !DASH
      real*8 A
      integer INDX, KODE, N
      character NAME*(*)
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external SINGD, HALT, HI, BYE
C
C               A(N), INDX(N)
      dimension A(*), INDX(*)
C
      call HI ('SORT')
C     !BEG
      call SINGD  (A, N, KODE, INDX)
C
      if(KODE.le.0) then
        write (MSSLIN(1),100) KODE,NAME,N
  100   format('KODE =',I5,'; unable to sort ',A,' of length',I10)
        call HALT ('SORT', 1)
      end if
C     !END
      call BYE ('SORT')
C
      return
      end
