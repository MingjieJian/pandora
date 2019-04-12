      subroutine HUPPI
     $(NO,IS,IE,LIMP,A,N,TIT)
C
C     Rudolf Loeser, 1979 Oct 17
C---- Prints arrays for HSE debugging.
C     IS and IE delimit a subset of A(I,J),
C     1 .le. IS .le. I .le. IE .le. N, with (IE-IS) .le. 7.
C     !DASH
      save
C     !DASH
      real*8 A
      integer I, IE, IS, J, LIMP, N, NO
      character TIT*(*)
C     !DASH
      external LINER, SHIM, HI, BYE
C
C               A(N,LIMP)
      dimension A(N,*)
C
      call HI ('HUPPI')
C     !BEG
      call LINER  (1,NO)
      write (NO,100) TIT,(I,I=IS,IE)
  100 format(' ',A3,8I15)
C
      do 102 J = 1,LIMP
        write (NO,101) J,(A(I,J),I=IS,IE)
  101   format(' ',I3,1P8E15.7)
        call SHIM (J,5,NO)
  102 continue
C     !END
      call BYE ('HUPPI')
C
      return
      end
