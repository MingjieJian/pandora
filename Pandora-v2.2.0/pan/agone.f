      subroutine AGONE
     $(MO,N,Z,TE,VM,PEX,PGS,EMN)
C
C     Rudolf Loeser, 2000 Jun 06
C---- Mach number printout, for HSE.
C     !DASH
      save
C     !DASH
      real*8 EMN, PEX, PGS, TE, VM, Z
      integer I, MO, N
      logical VMZERO
C     !DASH
      external ABJECT, LINER, NAUGHTD, HI, BYE
C
C               EMN(N), TE(N), VM(N), Z(N), PEX(N), PGS(N)
      dimension EMN(*), TE(*), VM(*), Z(*), PEX(*), PGS(*)
C
      call HI ('AGONE')
C     !BEG
      call NAUGHTD  (VM, 1, N, VMZERO)
      if((MO.gt.0).and.(.not.VMZERO)) then
        call ABJECT (MO)
        write (MO,100)
  100   format(' ','Mach number for mass motion velocity'///
     $             20X,'Z',14X,'TE',14X,'VM',13X,'PGS', 13X,'PEX',
     $             14X,'MN')
        call LINER  (1, MO)
        write (MO,101) (I,Z(I),TE(I),VM(I),PGS(I),PEX(I),EMN(I),I=1,N)
  101   format(5(' ',I5,1P6E16.8/))
      end if
C     !END
      call BYE ('AGONE')
C
      return
      end
