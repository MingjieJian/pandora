      subroutine SNAIL
     $(N,KSW,KSWD)
C
C     Rudolf Loeser, 2006 May 02
C---- Processes switch settings for SULTANA.
C     !DASH
      save
C     !DASH
      integer I, KSW, KSWD, N
C     !DASH
      external HI, BYE
C
C               KSW(N), KSWD(N)
      dimension KSW(*), KSWD(*)
C
      call HI ('SNAIL')
C     !BEG
      do 100 I = 1,N
        if(KSW(I).lt.0) then
          KSW(I) = 0
        else if(KSW(I).eq.0) then
          KSW(I) = KSWD(I)
        else
          KSW(I) = 1
        end if
  100 continue
C     !END
      call BYE ('SNAIL')
C
      return
      end
