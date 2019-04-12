      subroutine SPIREA
     $(N,MRHO,XINCH,WMX,WMN,SMP,WEIGHT,RHWL,ORHO,RHOO,RHOS,RHOJ,RHOW,
     $ IU,IL,KILROY)
C
C     Rudolf Loeser, 1980 May 02
C---- Sets up RHO weight, for ANEMONE.
C     !DASH
      save
C     !DASH
      real*8 CRIT, ORHO, RHOJ, RHOO, RHOS, RHOW, RHWL, SMP, WEIGHT, WMN,
     $       WMX, XINCH
      integer IL, IU, MRHO, N
      logical KILROY
C     !DASH
      external BUGLOSS, TRASH, SAFIRE, HI, BYE
C
C               RHOO(N), RHOS(N), RHOW(N), WEIGHT(N), RHWL(N), ORHO(N),
      dimension RHOO(*), RHOS(*), RHOW(*), WEIGHT(*), RHWL(*), ORHO(*),
C
C               RHOJ(N)
     $          RHOJ(*)
C
      data CRIT /1.D-8/
C
      call HI ('SPIREA')
C     !BEG
C---- Set up preliminary revised weights
      call BUGLOSS (MRHO, N, WEIGHT, RHWL, XINCH, WMX, WMN, SMP, ORHO,
     $              RHOO, RHOS, RHOJ, RHOW)
C---- Edit out junk
      call TRASH   (WEIGHT, N, CRIT)
C---- Save for iterative summary
      call SAFIRE  (IU, IL, WEIGHT, KILROY)
C     !END
      call BYE ('SPIREA')
C
      return
      end
