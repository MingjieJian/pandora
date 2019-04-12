      subroutine ALE
     $(N,K,DL,SNUU,DLS,SNUS,FL)
C
C     Rudolf Loeser, 1983 Sep 01
C---- Computes a shifted SNU.
C     FL is scratch storage.
C     !DASH
      save
C     !DASH
      real*8 DL, DLS, FL, FSL, R, SNUS, SNUU
      integer I, IRET, J, JS, K, N
C     !DASH
      external PARINT, HI, BYE
C
C               DL(KM), SNUU(*,KM), DLS(*), SNUS(*), FL(KM), R(4)
      dimension DL(*),  SNUU(N,*),  DLS(*), SNUS(*), FL(*),  R(4)
C
      call HI ('ALE')
C     !BEG
      do 101 I = 1,N
C
        do 100 J = 1,K
          FL(J) = log(SNUU(I,J))
  100   continue
C
        JS = 0
        call PARINT (DL, 1, FL, 1, K, DLS(I), FSL, 1, IRET, JS, R)
C
        SNUS(I) = exp(FSL)
  101 continue
C     !END
      call BYE ('ALE')
C
      return
      end
