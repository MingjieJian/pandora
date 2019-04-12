      subroutine SORTC
     $(A,N,INDX,NAME)
C
C     Rudolf Loeser, 2003 Sep 18
C---- Sorts an array (with its associated pointer array), but
C     aborts with an error message if sorting fails.
C
C     See also: SORTI and SORT.
C     !DASH
      save
C     !DASH
      integer INDX, KODE, N
      character A*(*), NAME*(*)
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external SINGC, HALT, HI, BYE
C
C               A(N), INDX(N)
      dimension A(*), INDX(*)
C
      call HI ('SORTC')
C     !BEG
      call SINGC  (A, N, KODE, INDX)
C
      if(KODE.le.0) then
        write (MSSLIN(1),100) KODE,NAME,N
  100   format('KODE =',I5,'; unable to sort ',A,' of length',I10)
        call HALT ('SORTC', 1)
      end if
C     !END
      call BYE ('SORTC')
C
      return
      end
