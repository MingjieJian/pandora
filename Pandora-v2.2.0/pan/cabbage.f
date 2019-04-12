      subroutine CABBAGE
     $(LU,KODE,N,K,IU,IL,Z,DL,XJNU)
C
C     Rudolf Loeser, 1980 Jun 12
C---- Writes (KODE=1) or reads (KODE=2) or skips (KODE=3)
C     values in a PRD Jnu restart file.
C     !DASH
      save
C     !DASH
      real*8 DL, XJNU, Z, dummy
      integer I, IL, IU, J, K, KODE, LU, N
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external HALT, HI, BYE
C
      dimension Z(N), DL(K), XJNU(N,K)
C
      call HI ('CABBAGE')
C     !BEG
      if((KODE.lt.1).or.(KODE.gt.3)) then
        write (MSSLIN(1),100) KODE
  100   format('KODE =',I12,', which is not 1, 2 or 3.')
        call HALT ('CABBAGE', 1)
      end if
C     !EJECT
  201 format(2I4)
  202 format(1P5E16.8)
  203 format(5E16.8)
C
      goto (101,102,103), KODE
  101 continue
        write (LU,202) Z,DL
        write (LU,201) IU,IL
        write (LU,202) XJNU
        goto 199
  102 continue
        IU = 0
        IL = 0
        read  (LU,203) Z,DL
        read  (LU,201) IU,IL
        read  (LU,203) XJNU
        goto 199
  103 continue
        IU = 0
        IL = 0
        read  (LU,203) (dummy,I=1,N),(dummy,J=1,K)
        read  (LU,201) IU,IL
        read  (LU,203) ((dummy,I=1,N),J=1,K)
        goto 199
C
  199 continue
C     !END
      call BYE ('CABBAGE')
C
      return
      end
