      subroutine CHROME
     $(K,L,A,KODE,SIG,YUL,YLL)
C
C     Rudolf Loeser, 1978 May 01
C---- Updates overall extrema, for a family of tables.
C     (This is version 3 of CHROME.)
C     !DASH
      save
C     !DASH
      real*8 A, SIG, YLL, YUL
      integer IMAX, IMIN, K, KODE, L
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external  MINMAXD, MNMXD, HALT, HI, BYE
      intrinsic min, max
C
C               A(K,L)
      dimension A(*)
C
      call HI ('CHROME')
C     !BEG
      if(KODE.eq.0) then
        call MINMAXD (A,1,(K*L),    IMIN,IMAX)
      else if(KODE.eq.1) then
        call MNMXD   (A,1,(K*L),SIG,IMIN,IMAX)
      else
        write (MSSLIN(1),100) KODE
  100   format('KODE =',I12,', which is neither 0 nor 1.')
        call HALT    ('CHROME',1)
      end if
C
      YUL = max(YUL,A(IMAX))
      YLL = min(YLL,A(IMIN))
C     !END
      call BYE ('CHROME')
C
      return
      end
