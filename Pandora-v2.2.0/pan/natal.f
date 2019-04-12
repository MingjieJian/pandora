      subroutine NATAL
     $(KAMB,N,HND,HE1,HEK,HE21,HE2K,HEND, XN)
C
C     Rudolf Loeser, 1997 Apr 29
C---- Computes "n" = XN, for s calculation.
C     !DASH
      save
C     !DASH
      real*8 CRIT, DIF, HE1, HE21, HE2K, HEK, HEND, HND, XN
      integer I, KAMB, N
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external  MOVE1, HALT, HI, BYE
      intrinsic abs
C
C               HEND(N), HND(N), HE1(N), HEK(N), HE21(N), HE2K(N), XN(N)
      dimension HEND(*), HND(*), HE1(*), HEK(*), HE21(*), HE2K(*), XN(*)
C
      data CRIT /1.D-4/
C     !EJECT
C
      call HI ('NATAL')
C     !BEG
      if(KAMB.eq.1) then
        call MOVE1 (HND, N, XN)
C
      else if(KAMB.eq.2) then
        do 100 I = 1,N
          DIF = HEND(I)-HE2K(I)
          if(abs(DIF).gt.(CRIT*HEND(I))) then
            XN(I) = DIF
          else
            XN(I) = HE1(I)+HEK(I)
          end if
  100   continue
C
      else if(KAMB.eq.3) then
        do 101 I = 1,N
          DIF = HEND(I)-HE1(I)
          if(abs(DIF).gt.(CRIT*HEND(I))) then
            XN(I) = DIF
          else
            XN(I) = HE21(I)+HE2K(I)
          end if
  101   continue
C
      else
        write (MSSLIN(1),102) KAMB
  102   format('KAMB =',I12,', which is not 1, 2 or 3.')
        call HALT  ('NATAL', 1)
      end if
C     !END
      call BYE ('NATAL')
C
      return
      end
