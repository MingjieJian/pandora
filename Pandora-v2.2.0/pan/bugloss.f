      subroutine BUGLOSS
     $(MRHO,N,WEIGHT,RHWL,XINCH,WMX,WMN,SMP,ORHO,RHOO,RHOS,RHOJ,RHOW)
C
C     Rudolf Loeser, 1980 May 02
C---- Sets up weights, according to "new" procedure, for SPIREA.
C     !DASH
      save
C     !DASH
      real*8 ORHO, RHOJ, RHOO, RHOS, RHOW, RHWL, SMP, WEIGHT, WMN, WMX,
     $       XINCH
      integer MRHO, N
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external ORCHID, HALT, HI, BYE
C
C               WEIGHT(N), RHWL(N), ORHO(N), RHOO(N), RHOS(N), RHOW(N),
      dimension WEIGHT(*), RHWL(*), ORHO(*), RHOO(*), RHOS(*), RHOW(*),
C
C               RHOJ(N)
     $          RHOJ(*)
C
      call HI ('BUGLOSS')
C     !BEG
      if(MRHO.eq.0) then
        call ORCHID (N, WEIGHT, RHWL, XINCH, WMX, WMN, SMP, ORHO, RHOO,
     $               RHOS)
C
      else if(MRHO.eq.1) then
        call ORCHID (N, WEIGHT, RHWL, XINCH, WMX, WMN, SMP, ORHO, RHOO,
     $               RHOJ)
C
      else if(MRHO.eq.2) then
        call ORCHID (N, WEIGHT, RHWL, XINCH, WMX, WMN, SMP, ORHO, RHOO,
     $               RHOW)
C
      else
        write (MSSLIN(1),100) MRHO
  100   format('MRHO =',I12,', which is not 0, 1, or 2.')
        call HALT   ('BUGLOSS', 1)
      end if
C     !END
      call BYE ('BUGLOSS')
C
      return
      end
