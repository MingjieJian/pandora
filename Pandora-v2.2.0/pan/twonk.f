      subroutine TWONK
     $(I,NW,N,WT,AMON,AINT,Z)
C
C     Rudolf Loeser, 1979 Nov 13
C---- Cumulates integrals over wavelength.
C     !DASH
      save
C     !DASH
      real*8 AINT, AMON, WT, Z
      integer I, N, NW
C     !DASH
      external ZERO1, TWACK, ARRINC, HI, BYE
C
C               AMON(N), AINT(N), WT(Nmkuse)
      dimension AMON(*), AINT(*), WT(*)
C
      call HI ('TWONK')
C     !BEG
      if(I.eq.1) then
        call ZERO1 (AINT,N)
      end if
C
      call TWACK   (I,NW,WT,Z)
      call ARRINC  (AMON,Z,AINT,N)
C     !END
      call BYE ('TWONK')
C
      return
      end
