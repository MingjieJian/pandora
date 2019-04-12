      subroutine BANOIC
     $(N,M,CEK,NO)
C
C     Rudolf Loeser, 1980 Mar 14
C---- Prints CHECKS-"M", for BANQUE.
C     !DASH
      save
C     !DASH
      real*8 CEK, DELTA, ONE
      integer K, KOUNT, L, M, N, NO
      character GOOD*32
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external RANGED, LINER, BRATEN, HI, BYE
C
C               CEK(N)
      dimension CEK(*)
C
      data GOOD  /'= 1.0 to eight figures or better'/
      data DELTA /1.D-8/
C
      call HI ('BANOIC')
C     !BEG
      K = M-2
      L = M-1
      call LINER    (1, NO)
      write (NO,100) M, L,K, M,L, K,M
  100 format(' ','CHECK',I2,', from ',3('(',I2,'/',I2,')'),
     $           2X,9('----------'))
      call LINER    (1, NO)
C
      call RANGED   (CEK, 1, N, DELTA, ONE, KOUNT)
      if(KOUNT.ge.N) then
        write (NO,101) GOOD
  101   format(' ',25X,A32)
      else
        call BRATEN (CEK, N, NO)
      end if
C     !END
      call BYE ('BANOIC')
C
      return
      end
