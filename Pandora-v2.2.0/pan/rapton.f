      subroutine RAPTON
     $(N,KAMB,KVLG,HEND,HEK,SHE,HE2K,SHE2,DIONL,DLVSL)
C
C     Rudolf Loeser, 1999 Jun 11
C---- Computes "FIONL" and "FLVSL" (for CENSUS).
C     (This is version 2 of RAPTON.)
C     !DASH
      save
C     !DASH
      real*8 DIONL, DLVSL, HE2K, HEK, HEND, SHE, SHE2, SIG, ZERO
      integer I, KAMB, KION, KVLG, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external  ARRDIV, LOGO, HI, BYE
      intrinsic max
 
C
C               HEND(N), HEK(N), DLVSL(N), HE2K(N), SHE2(N), DIONL(N),
      dimension HEND(*), HEK(*), DLVSL(*), HE2K(*), SHE2(*), DIONL(*),
C
C               SHE(N)
     $          SHE(*)
C
      data SIG /3.D2/
C
      call HI ('RAPTON')
C     !BEG
      KION = max(KAMB,KVLG)
      if((KION.eq.2).or.(KION.eq.3)) then
        if(KION.eq.2) then
          call ARRDIV (HEK,  HEND, DIONL, N)
          call ARRDIV (SHE,  HEND, DLVSL, N)
        else
          call ARRDIV (HE2K, HEND, DIONL, N)
          call ARRDIV (SHE2, HEND, DLVSL, N)
        end if
C
        call LOGO     (DIONL, N, 1, SIG, DIONL)
        call LOGO     (DLVSL, N, 1, SIG, DLVSL)
      end if
C     !END
      call BYE ('RAPTON')
C
      return
      end
