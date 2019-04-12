      subroutine EMERY
     $(HND,Z,N,CVZ)
C
C     Rudolf Loeser, 1993 Jan 28
C---- Computes default value for CVZ, a fluid velocity parameter.
C     !DASH
      save
C     !DASH
      real*8 CRIT, CVZ, HND, Z
      integer I, N
C     !DASH
      external HI, BYE
C
C               HND(N), Z(N)
      dimension HND(*), Z(*)
C
      data CRIT /1.D11/
C
      call HI ('EMERY')
C     !BEG
      do 100 I = 1,N
        if(HND(I).gt.CRIT) then
          CVZ = Z(I)
          goto 101
        end if
  100 continue
C
      CVZ = Z(N)
C
  101 continue
C     !END
      call BYE ('EMERY')
C
      return
      end
